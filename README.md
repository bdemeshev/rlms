[![Travis-CI Build Status](https://travis-ci.org/bdemeshev/rlms.svg?branch=master)](https://travis-ci.org/bdemeshev/rlms)

rlms
====

Пакет `rlms` предназначен для работы с данными  [исследования RLMS](http://www.hse.ru/rlms/) в R. Пакет можно установить командами:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
```
Известна одна ситуация, в которой пакет `rlms` не ставится при выполнении этих команд: давно необновлённые другие пакеты, в частности, пакет `foreign`. В Rstudio пакеты можно обновить через меню `Tools`-`Check for package updates`. Можно обновить пакет и вручную, введя команду `install.packages("foreign")` (вместо `foreign` можно написать имя другого старого пакета). Пакеты устанавливается один раз, повторять эти две команды каждый раз при работе с данными RLMS не нужно :)


Переходим к работе! Данные RLMS можно скачать с [официального сайта RLMS](http://www.hse.ru/rlms/spss). А далее --- загрузить командой:
```r
library("rlms")
df <- rlms_read("r21i_os24a.sav")
```

При загрузке данных RLMS возникает вопрос, как конвертировать меченные (labelled) переменные, которые использует SPSS, в факторные (factor) или числовые (numeric), которые использует R. Меченная переменная, это числовая переменная, у которой к некоторым, возможно ко всем значениям, добавлены текстовые описания (метки). По умолчанию функция `read_rlms()` пытается сама определить во что лучше перевести меченные переменные:

* Если все значения помечены, то переменная переводится в факторную. 
* Если помечены все значения кроме больших 99999990, то переменная переводится в факторную. 
* Бывает и такое: у одного значения забыта метка. Если у переменной единственное непомеченное значение, причем это непомеченное значение не является ни минимумом, ни максимумо, то такая переменная переводится в факторную. В оставшихся случаях переменная переводится в численную.

Можно сохранить меченные переменные как они есть:
```r
df <- rlms_read("r21i_os24a.sav", haven = "labelled")
```

Пакет содержит ряд функций для работы с меченными переменными: `is_labelled()`, `all_labelled()`, `get_labels()`, `get_label()`, ...

Также можно сохранить значения всех меченных переменные в виде численных кодов:
```r
df <- rlms_read("r21i_os24a.sav", haven = "numeric")
```



Если по каким-то причинам нужно читать файл с помощью пакета `foreign`, то:
```r
df <- rlms_read_legacy("r21i_os24a.sav")
```


Пакет автоматически сохраняет метаданные о названии переменных:
```r
var_meta <- rlms_show_variable_labels(df)
head(var_meta)
```

Метаданные о помеченных значениях:
```r
value_meta <- rlms_show_value_labels(df)
head(value_meta)
```

Авторы рекомендуют использовать пакет `dplyr` для дальнейшей обработки данных. 
В частности, загружаемому набору данных автоматически добавляется класс `tbl`, чтобы избежать существенных затрат времени при случайной команде вывести весь массив `df` на экран:

```r
library("dplyr")
df
```

При работе с панельными данными удобно сначала отконвертировать все `.sav` файлы в `.Rds` формат. Это ускоряет функцию `rlms_load()`:

```r
rlms_sav2rds("~/Downloads/Все выборки/")
```

Примечание: Для корректной распаковки архива с русскими буквами (используется кодировка CP866) на маке можно воспользоваться архиватором [the Unarchiver](http://unarchiver.c3.cx/).


Можно загружать данные по номеру волны: 

```r
df <- rlms_load("~/Downloads/Все выборки/", 
  wave = 20, sample = "all", level = "individual")
```

Расшифровка имени файла:
```r
rlms_fileinfo("r10hall23.sav")
```



Планы:

- [x] синтаксис стиль по [Wickham style guide](http://adv-r.had.co.nz/Style.html), используем  lint() из пакета `lintr`
- [x] travis ci тестирование
- [x] удалять автоматом level == "" если таких данных нет (без этого возникает warning duplicate levels), другие отсутствующие уровни удалять опционально
- [x] унифицировать ДА, да, Да, etc в кодах переменных. опция
- [x] загрузка через haven - опция
- [ ] пустую метку без данных удаляем, пустую метку с данными заменяем на значение
- [ ] сохранение протокола преобразований
- [ ] удаление повторных значений в labels
- [ ] во встроенный waves_info добавить n_obs, n_vars, год волны? что-то ещё? 
- [ ] встроенный мини sample из последней волны для учебных и демо-целей ?
- [ ] проверить, есть ли особенности обработки модуля по женщинам 19 волны
- [ ] документация!!!! пример построения простой панели с fe/re/pooled
- [ ] слияние нескольких волн в панель. Проект функции. на входе: указываются волны, переменные, на выходе: панелька под `plm`. Пакет `psData` похоже не активен. 
- [ ] слияние индивидуальных и семейных файлов. Проект функции. ??? Похоже предыдущие пункты --- это должна быть __единая__ функция, см [psidR](https://github.com/floswald/psidR/)
- [ ] задача типа: подклеить зп мужа/жены/сестер в индивидуальный опросник. Подклеить с лагом?

Синтаксис в жанре ???
```r
df <- rlms_ipanel(wave = 5:22,
  ~ educ + father(educ) + spouse(income + educ) +  spouse(income, lag = 1))
```
не изобретать велосипед про лаги --- воспрос в типе подклейки
понятия родственников из rlms и очевидные обобщения (cibling, family, spouse)
если родственников несколько --- простая агрегирующая функция (sum/mean) или всех в таблицу?



### English translation

The `rlms`-package is designed to facilitate the processing of [RLMS data](http://www.hse.ru/rlms/) in  R. The package may be installed via:
```r
install.packages("devtools")
devtools::install_github("bdemeshev/rlms")
```

Data may be loaded with one simple command:
```r
library("rlms")
df <- rlms_read("r21i_os24a.sav")
```

The package automatically saves the metadata about variable full names:
```r
var_meta <- rlms_show_variable_labels(df)
head(var_meta)
```

Metadata on cathegory values:
```r
value_meta <- rlms_show_value_labels(df)
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
df <- rlms_load("~/Downloads/Все выборки/", 
  wave = 20, sample = "all", level = "individual")
```

Or decipher filename:
```r
rlms_fileinfo("r10hall23.sav")
```


