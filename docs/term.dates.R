term.dates <- function(init_buy_dt, terms_mat){
    ###
    ### input ###
    # init_buy_dt: 고객별 초기 날짜 값 벡터
    # terms_mat: 다음 구매까지 걸린 일수 매트릭스
    ###
    ### output ###
    # result: 초기 날짜 값과 다음 구매 걸린 일 수를 더한 날짜 데이터 프레임
    ###
    library(tidyverse)
    library(lubridate)
    
    init_buy_dt <- ymd(init_buy_dt)
    
    init_buy_dt_plus_terms <- 
        terms_mat %>% 
        apply(2, cumsum) %>% 
        t() + init_buy_dt
    
    result <- 
        c(init_buy_dt, init_buy_dt_plus_terms) %>% 
        as.character() %>% 
        matrix(ncol = 3, byrow = T) %>%
        as.data.frame() 
    
    colnames(result) <- colnames(terms) 
    
    return(result)
}
