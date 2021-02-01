library(dplyr)
library(rvest)
library(jsonlite)
library(RSelenium)

# CLEAR WORKSPACE, CLEAR PLOTS, AND SET DIRECTORY
rm(list = ls())
graphics.off()
dirpath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirpath)


# FUNCTIONS ---------------------------------------------------------------

# FUNCTION THAT SEARCHES FOR PARTIAL NAMES IN NODE ATTRIBUTES
XPathPartialClass <- function(name, container = "div", attribute = "class") {
  return(
    paste0(
      '//',
      container,
      '[@',
      attribute,
      ' and contains(concat(" ", normalize-space(@',
      attribute,
      '), " "),"',
      name,
      '")]'
    )
  )
}


# FUNCTION THAT SCRAPES PRODUCT DATA
ProductOnline <- function(link) {
  scripts <- link %>%
    read_html() %>%
    html_nodes("script")
  
  for (j in 1:length(scripts)) {
    extracted_json <- scripts %>%
      .[j] %>%
      html_text(trim = TRUE)
    
    if (grepl("^window.__", extracted_json)) {
      break
    }
    
  }
  object_json <- extracted_json %>%
    gsub("^window.*window.*__=|[;]$", "", .) %>%
    fromJSON(simplifyDataFrame = TRUE) %>%
    {.$apolloState$data}
  
  avl_online <- object_json %>% 
    {.[grepl("ProductVariant.*attributes", names(.))][[1]]} %>%
    .[["canAddToBag"]]
  avl_details <- object_json %>% 
    {.[grepl("ProductVariant.*attributes", names(.))][[1]]} %>%
    .[["availabilityText"]]
  order_limit <- object_json %>% 
    {.[grepl("ProductVariant.*attributes", names(.))][[1]]} %>%
    .[["maxOrderQuantity"]]
  price <- object_json %>% 
    {.[grepl("ProductVariant.*listPrice", names(.))][[1]]} %>%
    .[["formattedValue"]]
  vip_pts <- object_json %>% 
    {.[grepl("^ProductVariant", names(.))][[1]]} %>%
    .[["vipPoints"]]
  product_name <- object_json %>% 
    {.[grepl("SingleVariantProduct", names(.))][[1]]} %>%
    .[["name"]] %>%
    gsub("\231","",.) # remove TM symbol
  product_code <- object_json %>%
    {.[grepl("SingleVariantProduct", names(.))][[1]]} %>%
    .[["productCode"]]
  
  df <- data.frame(
    "ITEM" = product_code,
    "NAME" = product_name,
    "PRICE" = price,
    "VIP" = vip_pts,
    "TYPE" = "Online",
    "SOURCE" = "LEGO.com",
    "INSTOCK" = avl_online,
    "DETAILS" = avl_details,
    "ADDRESS" = link,
    "TIMESTAMP" = if_else(avl_online, Sys.time(), as.POSIXct(NA))
  )
  
  return(df)
}


# FUNCTION THAT GETS VERSION OF CHROME (MIGHT BE REQUIRED FOR SELENIUM)
ChromeVersion <- function() {
  return(
    system2(
      command = "wmic",
      args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
      stdout = TRUE,
      stderr = TRUE
    ) %>%
      stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
      magrittr::extract(!is.na(.)) %>%
      stringr::str_replace_all(pattern = "\\.",
                               replacement = "\\\\.") %>%
      paste0("^",  .) %>%
      stringr::str_subset(
        string =
          binman::list_versions(appname = "chromedriver") %>%
          dplyr::last()
      ) %>%
      as.numeric_version() %>%
      max() %>%
      as.character()
  )
}


# INVENTORY AND STORE DETAILS
StoreStatus <- function() {
  store_name <- remDr$findElement(using = "xpath", '//div[@data-test = "store-inventory-preview-name"]')$
    getElementText() %>% unlist()
  store_status <- remDr$findElement(using = "xpath", '//div[@data-test = "store-inventory-preview-status"]')$
    getElementText() %>% unlist()
  store_address <- remDr$findElement(using = "xpath", '//div[@data-test = "store-inventory-preview-address"]')$
    getElementText() %>% unlist()
  
  store_bool <- grepl("in stock", store_status, ignore.case = TRUE)
  
  return(
    data.frame(
      "TYPE"="Retail",
      "SOURCE"=store_name,
      "INSTOCK"=store_bool,
      "DETAILS"=store_status,
      "ADDRESS"=store_address,
      "TIMESTAMP"=if_else(store_bool,Sys.time(),as.POSIXct(NA))
    )
  )
}


# GET ALL STORE DATA FOR THE NUMBER OF STORES SPECIFIED
StoreData <- function(store_num) {
  store_i <- 1
  df <- StoreStatus()
  
  while (store_i < store_num) {
    # CHANGE STORE
    Sys.sleep(0.5)
    remDr$findElement(using = "xpath", '//div[@data-test = "store-inventory-selected-store"]')$clickElement()
    Sys.sleep(0.5)
    remDr$findElement(using = "xpath", paste0('//div[@data-test = "store-inventory-store-unselected"][',store_i,']'))$clickElement()
    store_i <- store_i + 1
    
    df <- rbind(df, StoreStatus())
  }
  
  return(df)
}


# COMBINE ONLINE AND IN STORE STATUS
TotalData <- function(link, store_num) {
  df <- bind_rows(ProductOnline(link), StoreData(store_num))
  df[c("ITEM", "NAME", "PRICE", "VIP")] <- df[1, c("ITEM", "NAME", "PRICE", "VIP")]
  
  return(df)
}

# FUNCTION THAT RESETS PORTS
ResetPorts <- function() {
  invisible(capture.output(
    system(
      "taskkill /im java.exe /f",
      intern = FALSE,
      ignore.stdout = FALSE,
      show.output.on.console = FALSE
    )
  ))
}

# ALL SCRAPING INTO ONE MASTER FUNCTION
MasterScraper <- function(links, zip_code, store_num=3) {
  
  # RESET PORTS
  ResetPorts()
  
  pb <- txtProgressBar(min=0, max=length(links), style=3)
  link_i <- 1
  if (store_num > 5) {store_num <- 5}
  
  # OPEN CONNECTION TO INTERNET
  rD <- rsDriver(
    browser = "chrome",
    chromever = ChromeVersion(),
    verbose = FALSE,
    extraCapabilities = list("chromeOptions" = list(args = list('--headless'))) # runs without opening window
  )
  remDr <<- rD$client
  
  # GO TO FIRST PRODUCT
  remDr$navigate(links[link_i])
  Sys.sleep(2)
  
  # SET UP PAGE FOR ZIP CODE INPUT
  remDr$findElement(using = "xpath", '//button[@data-test = "age-gate-grown-up-cta"]')$clickElement()
  remDr$findElement(using = "xpath", '//button[@data-test = "cookie-banner-normal-button"]')$clickElement()
  
  # remDr$findElement(using = "xpath", '//button[@data-test = "accordion-title"]')$clickElement()
  remDr$findElement(using = "xpath", XPathPartialClass("stock-accordion", "button", "data-test"))$clickElement()
  
  # INPUT ZIP CODE
  remDr$findElement(using = "xpath", '//input[@data-test = "input-with-button-input"]')$sendKeysToElement(list(zip_code))
  remDr$findElement(using = "xpath", '//button[@data-test = "input-with-button-button"]')$clickElement()
  Sys.sleep(0.5)
  
  # GET STOCK STATUS FOR BOTH ONLINE AND IN STORE OPTIONS
  df <- TotalData(links[link_i], store_num)
  setTxtProgressBar(pb, link_i)
  
  # NAVIGATE TO NEXT PRODUCTS
  while (link_i < length(links)) {
    link_i <- link_i + 1
    
    remDr$navigate(links[link_i])
    Sys.sleep(0.5)
    remDr$findElement(using = "xpath",
                      XPathPartialClass("stock-accordion", "button", "data-test"))$clickElement()
    Sys.sleep(1.0)
    
    df <- rbind(df, TotalData(links[link_i], store_num))
    setTxtProgressBar(pb, link_i)
  }
  
  # CHANGE COLUMNS WITH FACTORS TO CHARACTERS
  df <- df %>%
    mutate_if(is.factor, as.character)
  
  # CLOSE CONNECTION
  remDr$quit()
  ResetPorts()
  
  return(df)
}

# MAIN --------------------------------------------------------------------

# PROVIDE LINKS TO PRODUCTS
links <- c(
  "https://www.lego.com/en-us/product/501st-legion-clone-troopers-75280",
  "https://www.lego.com/en-us/product/luke-skywalker-s-x-wing-fighter-75301",
  "https://www.lego.com/en-us/product/imperial-tie-fighter-75300"
)

# GET ALL PRODUCTS AND THEIR INVENTORY STATUS
products_df <- MasterScraper(links, zip_code = "92612", store_num = 3) %>%
  .[!duplicated(.[,grepl("ITEM|SOURCE",colnames(.))]),]

write.csv(products_df, "./demo_data.csv", row.names = FALSE)
