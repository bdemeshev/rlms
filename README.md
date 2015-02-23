rlms
====

Пакет `rlms` предназначен для работы с данными  [исследования RLMS](http://www.hse.ru/rlms/) в R. Данная документация относится к версии 0.4. Пакет можно установить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
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

Авторы рекомендуют использовать пакет `dplyr` для дальнейшей обработки данных. 
В частности, загружаемому набору данных автоматически добавляется класс `tbl`, чтобы избежать существенных затрат времени при случайной команде вывести весь массив `df` на экран:

```r
library("dplyr")
df
```

При работе с панельными данными удобно сначала отконвертировать все `.sav` файлы `.Rds` формат. Это ускоряет функцию `rlms_load()`

```r
rlms_sav2rds("~/Downloads/Все выборки/")
```

Примечание: Для корректной распаковки архива с русскими буквами (используется кодировка CP866) на маке можно воспользоваться архиватором [the Unarchiver](http://unarchiver.c3.cx/).


Можно загружать данные по номеру волны: 

```r
df <- rlms_load("~/Downloads/Все выборки/", wave=20, sample="all", level="individual")
```

Расшифровка имени файла:
```r
rlms_fileinfo("r10hall23.sav")
```



Планы:


- [ ] слияние нескольких волн в панель. Проект функции. на входе: указываются волны, переменные, на выходе: панелька под `plm`. Пакет `psData` похоже не активен. 
- [ ] слияние индивидуальных и семейных файлов. Проект функции. ??? Похоже пункты 1 и 2 это должна быть __единая__ функция, см [psidR](https://github.com/floswald/psidR/)
- [ ] встроенный data.frame с информацией по волнам ? (туда имя файла включить)
- [ ] проверить, есть ли особенности обработки модуля по женщинам 19 волны
- [ ] унифицировать ДА, да, Да, etc в кодах переменных. опция

- добавить в документацию примеры:
- [ ] пример построения простой панели с fe/re/pooled

- по картам:
- [ ] EPSG Projection 3413
- [ ] ортографическая проекция 
- [ ] три уровня административного деления
- [ ] наилучший источник карт?
- [ ] карта отдельного региона


### English translation

The `rlms`-package is designed to facilitate the processing of [RLMS data](http://www.hse.ru/rlms/) in  R. Current documentation refers to version 0.4. The package may be installed via:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
```

Data may be loaded with one simple command:
```r
library("rlms")
df <- read.rlms("r21i_os24a.sav")
```

The package automatically saves the metadata about variable full names:
```r
var_meta <- attr(df,"var_meta")
head(var_meta)
```

Metadata on cathegory values:
```r
value_meta <- attr(df, "value_meta")
head(value_meta)
```

Authors recommend to use the package `dplyr` for further data processing. 
In particular, to avoid occasional printing of whole data.frame the class `tbl` is 
automatically added:

```r
library("dplyr")
df
```

It may be convenient to convert all `.sav` files to `.Rds` format. This will speed up the  `rlms_load()` function.

```r
rlms_sav2rds("~/Downloads/Все выборки/")
```

Note: To correctly extract cyrillic folder names (CP866 encoding is used) on mac os one may use the free [Unarchiver](http://unarchiver.c3.cx/).


One may load data by wave number: 
```r
df <- rlms_load("~/Downloads/Все выборки/", wave=20, sample="all", level="individual")
```

Or decipher filename:
```r
rlms_fileinfo("r10hall23.sav")
```


