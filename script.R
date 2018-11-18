# java -jar selenium-server-standalone-3.9.1.jar

library(tidyverse)
library(tibble)
library(lubridate)
library(RSelenium)

server <- 
  remoteDriver(
    remoteServerAddr = 'localhost',
    port = 4444L,
    browserName = 'chrome'
  )

parameters <- 
  tibble(
    url = 
      c(
        'https://www.fastshop.com.br/web/p/d/AEMP852BRARSA_PRD/iphone-se-rosa-dourado-com-tela-de-4-4g-32-gb-e-camera-de-12-mp-mp852bra',
        'https://www.fastshop.com.br/web/p/d/AEMN0W2BRACNZ_PRD/iphone-6s-cinza-espacial-com-tela-de-47-4g-32-gb-e-camera-de-12-mp-mn0w2bra',
        'https://www.fastshop.com.br/web/p/d/AEMN0X2BRAPTA_PRD/iphone-6s-prata-com-tela-de-47-4g-32-gb-e-camera-de-12-mp-mn0x2bra',
        'https://www.submarino.com.br/produto/129251732/iphone-6s-32gb-cinza-tela-retina-hd-4-7-3d-touch-camera-12mp-apple?pfm_carac=iphone%206s&pfm_index=0&pfm_page=search&pfm_pos=grid&pfm_type=search_page%20',
        'https://www.magazineluiza.com.br/iphone-6s-apple-32gb-cinza-espacial-4g-tela-4-7-retina-cam-12mp-selfie-5mp-ios-11-proc-a9/p/216972900/te/teip/',
        # 'https://www.carrefour.com.br/iPhone-6S-32GB-Cinza-Espacial-4G-Tela-4-7-Camera-12MP-IOS/p/5038480',
        'https://www.extra.com.br/TelefoneseCelulares/Smartphones/iPhone/iphone-6s-apple-com-3d-touch-ios-11-sensor-touch-id-camera-isight-12mp-wi-fi-4g-gps-bluetooth-e-nfc-32gb-prateado-tela-47-10404707.html?rectype=p1_op_s2&recsource=btermo'  
      ),
    xpath_price = 
      c(
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/div/div[3]/div/div/div/div[1]/app-product-price/div/div/div[1]/p[2]/span',
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/div/div[3]/div/div/div/div[1]/app-product-price/div/div/div[1]/p[2]/span',
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/div/div[3]/div/div/div/div[1]/app-product-price/div/div/div[1]/p[2]/span',
        '//*[@id="content"]/div/main/section/div/div/div[2]/section[2]/section/div/ul/li[2]/div/p/strong',
        '/html/body/div[3]/div[4]/div[1]/div[4]/div[2]/div[4]/div/div[2]/div/span[2]',
        # '/html/body/main/div/div[6]/div/div[1]/div[2]/div/div[2]/div[3]/div/div/div[1]/div[2]/span/div/span/item',
        '//*[@id="ctl00_Conteudo_ctl61_divOutrasInfos"]/span/span[1]'
      ),
    xpath_product = 
      c(
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/h1',
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/h1',
        '//*[@id="body"]/div/div[4]/app-detail/div/div[2]/div[1]/div/h1',
        '//*[@id="content"]/div/main/section/div/div/div[2]/section[1]/h1',
        '/html/body/div[3]/div[4]/div[1]/div[3]/h1',
        # '//*[@id="productTop"]/div/div[1]/h1',
        '//*[@id="ctl00_Conteudo_upMasterProdutoBasic"]/div[2]/h1/b'
      )
  )

# result_prev <- 
#   tibble(
#     store = character(nrow(parameters)),
#     product = character(nrow(parameters)),
#     value = numeric(nrow(parameters))
#   )

result_all <- readRDS('result.rds')
result_prev <- tail(result_all, nrow(parameters))

while(today() <= as_date('2018-11-24')) {
  
  server$open(silent = T)
  
  timestamp()
  message('Coletando...')
  
  result <- 
    pmap(
      parameters,
      function(url, xpath_price, xpath_product) {
        # print(url)
        server$navigate(url)
        value <- server$findElement('xpath', xpath_price)
        while(!value$isElementDisplayed() %>% unlist()) { Sys.sleep(1) }
        dat <- 
          tibble(
            store = url %>% str_extract('www\\.([[:graph:]])*\\.com\\.br'),
            product = server$findElement('xpath', xpath_product)$getElementText() %>% unlist(),
            value = value$getElementText() %>% 
              unlist() %>% 
              str_remove('\\.') %>% 
              str_replace(',', '.') %>% 
              str_extract('([0-9]*\\.[0-9]{2})') %>% 
              as.numeric()
          )
        # print(dat)
        return(dat)
      }
    ) %>% 
    bind_rows()
  
  server$close()
  
  message('Conluido.')
  
  result$diff <- result$value - result_prev$value
  result_all <- rbind(result_all, result %>% add_column(date = now()))
  saveRDS(result_all, 'result.rds')
  
  if (any(result$diff != 0)) {
    if (any(result$diff < 0)) {
      message('\nREDUÇÃO DE PREÇO!!!\nREDUÇÃO DE PREÇO!!!\nREDUÇÃO DE PREÇO!!!\n')
    }
    if (any(result$diff > 0))
      message('Aumento de preço.')
  } else {
    message('Nenhuma alteração nos preços.')
  }
  print(result)
  result_prev <- result
  message('Aguardando...')
  Sys.sleep(15 * 60)
}
