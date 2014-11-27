rlms
====

Пакет `rlms` предназначен для работы с данными  [исследования RLMS](http://www.hse.ru/rlms/) в R. Пакет можно установить командами:
```r
install.packages("devtools")
library("devtools")
install_github("bdemeshev/rlms")
```

Данные можно загрузить командой:
```r
library("rlms")
df <- read.rlms("r21i_os24a.sav")
```

Пакет автоматически сохраняет метаданные о названии переменных:
```r
var_meta <- attr(df,"var_meta")
head(var_meta)
```

Метаданные об особых значениях:
```r
value_meta <- attr(df, "value_meta")
head(value_meta)
```

Планы:
* функция для слияния нескольких волн в панель

