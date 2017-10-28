#' Formats proper Polish numeral with nouns
#'
#' @export
liczebnik<-function(liczba, mianownik, dopelniacz, lmnoga, flag_liczby_slowami=FALSE, flag_skip_one=FALSE, flag_skip_zero=TRUE) {
  if (liczba==0 && flag_skip_zero) {
    return('')
  }
  if (flag_skip_one && liczba ==1 ) {
    return(mianownik)
  }
  if(flag_liczby_slowami) {
    out<-liczba_slowami(liczba)
  } else {
    out<-format(liczba, big.mark = '\uA0')
  }
  do_stu <- liczba %% 100
  if(do_stu==1 && liczba < 100) {
    out<-paste0(out, " ", mianownik)
  } else if (do_stu<5 && do_stu>0) {
    out<-paste0(out, " ", dopelniacz)
  } else if (do_stu<22) {
    out<-paste0(out, " ", lmnoga)
  } else {
    cyfra <- liczba %% 10
    if (cyfra <2 ) {
      out <- paste0(out, " ", lmnoga)
    } else if (cyfra < 5) {
      out <- paste0(out, " ", dopelniacz)
    } else {
      out <- paste0(out, " ", lmnoga)
    }
  }
  return(out)
}

#' This says the number in Polish language
#'
#' @export
liczba_slowami<-function(liczba, flag_skip_zero=FALSE) {
  if(liczba<0) {
    stop("Ujemne liczebniki nie są zaimplementowane")
  }
  if(length(liczba)>1)
  {
    stop("Nie można podać więcej, niż jednej liczby")
  }
  if(liczba!=as.integer(liczba)) {
    stop("Ułamki nie są zaimplementowane")
  }
  if (flag_skip_zero && liczba ==0 ) {
    return("")
  }

  cyfry<-c("zero","jeden","dwa", "trzy", "cztery", "pięć", "sześć", "siedem", "osiem", "dziewięć",
           "dziesięć", "jedenaście", "dwanaście", "trzynaście", "czternaście", "piętnaście", "szesnaście", "siedemnaście", "osiemnaście", "dziwiętnaście")
  dziesiątki <- c("dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt", "sześćdziesiąt", "siedemdziesiąt", "osiemdziesiąt", "dziewięćdziesiąt")
  setki <- c("sto", "dwieście", "trzysta", "czterysta", "pięćset", "sześćset", "siedemset", "osiemset", "dziwięćset")

  if(liczba > 10E24) {
    stop("Największa zaimplementowana liczba to 999 * 10^21")
  }

  if(liczba < 20) {
    return(cyfry[[liczba+1]])
  }

  if(liczba < 999) {
    c3 <- liczba %/% 100
    liczba <- liczba - 100 * c3
    c2 <- liczba %/% 10
    c1 <- liczba - 10 * c2
    if(c3>0) {
      out<-paste0(setki[[c3]], " ")
    }
    if(liczba<20) {
      return(paste0(out, cyfry[[liczba]]))
    }
    if(c1==0) {
      return(paste0(out, dziesiątki[[c2-1]]))
    } else {
      return(paste0(out, dziesiątki[[c2-1]], " ", cyfry[[c1+1]]))
    }
  }



  miliony1 <- c("tysiąc", "milion", "miliard", "bilion", "biliard", "trylion", "tryliard")
  miliony2 <- c("tysiące", "miliony", "miliardy", "biliony", "biliardy", "tryliony", "tryliardy")
  miliony3 <- c("tysięcy", "milionów", "miliardów", "bilionów", "biliardów", "trylionów", "tryliardów")
  zera <- c(1,seq(3,length(miliony1)*3, 3))
  out<-''
  liczba=1234567890
  while(liczba >0 ) {
    wykl <- as.integer(log10(liczba))+1
    pos<-which(zera-wykl>=0)[[1]]
    c4 <- liczba %/% 10^zera[[pos-1]]
    if (nchar(out)>0) {
      out <- paste0(out, " ")
    }
    if(liczba > 1000) {
      out <- paste0(out, liczebnik(liczba = c4, mianownik = miliony1[[pos-2]], dopelniacz = miliony2[[pos-2]], lmnoga = miliony3[[pos-2]], flag_liczby_slowami = TRUE, flag_skip_one=TRUE))
    } else {
      out <- paste0(out, liczba_slowami(liczba, flag_skip_zero = TRUE))
    }
    liczba <- liczba - c4 * 10^zera[[pos-1]]
  }
  return(out)
}
