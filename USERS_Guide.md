# GTF 
## Программный комплекс анализа приливов и планетарных волн в модели GSM TIP
## Содержание


## Описание

Программный комплекс предназначен для обработки результатов моделирования Глобальной самосогласованной модели термосферы, ионосферы и протоносферы (ГСМ ТИП) с целью определения параметров планетарных и приливных волн, их спектрального анализа и выделения из данных моделирования отдельных гармоник. Комплекс включает в себя несколько специализированных программ, реализованных на языке FORTRAN77, объединённых в один исполняемый файл.

### Системные требования

Устойчивая работа комплекса обеспечивается на 64-разрядных операционных системах семейства Windows. Обработка данных осуществляется без распараллеливания вычислений на различные ядра процессора, а потому достаточно требовательна к частоте одного ядра. Продолжительность вычислений прямо зависит от объёмов обрабатываемых данных и, в случае обработки десятков дней, может достигать нескольких часов на недостаточно производительных машинах.

### Входные параметры

Комплекс предназначен для работы с почасовыми данными о параметрах среды, получаемыми из ГСМ ТИП. Набор данных за каждый день в формате часовых файлов file4.01, file4.02, file4.03 … file4.24 должен быть помещён в номерованные папки с именами, соответствующими номерам дней, по типу 01, 02, 03 … 31. Предусмотрена возможность считывать данные из файлов, пронумерованных по часам в прямом порядке, без дополнительных папок по дням. Выбор способа чтения происходит в блоке TIDE. Путь к каталогу с этими папками должен содержать только стандартные символы и латинские буквы и не превышать 120 знаков.

Все генерируемые программным комплексом данные сохраняются в директории с исходным набором данных от ГСМ ТИП. В связи с этим, в этой директории должны быть открыты права на чтение и запись файлов, а также иметься достаточно свободного дискового пространства для записи нескольких десятков мегабайт данных.

Алгоритмы работы комплекса достаточно универсальны, чтобы обрабатывать любые произвольные наборы данных, структурированные определённым образом. Однако, при работе с данными не из файлов типа file4 необходимо использовать альтернативный способ ввода данных. Настоящая сборка программы ориентирована на её использование для обработки данных, полученных с помощью GSMTIPViewer, за январь 2009 года. Для этого были добавлены 22, 23 и 24 параметры, а в блоках GTF и TIDE\_alt указан соответствующий путь к папке с данными. В случае, если программу предполагается использовать только для анализа стандартных 19 параметров из file4 никаких изменений вносить не требуется, дополнительные параметры будут игнорироваться. В случае, если программу предполагается использовать для анализа данных из других источников, потребуется существенное изменение указанных блоков.

### Ввод параметров

Параметры обработки данных вводятся в файл info.txt в следующем порядке:

Количество атмосферных параметров, количество узлов по высоте, долготе, широте, по времени, технический параметр npril, равный 2, шаг сетки по широте, шаг сетки по долготе;

Признаки запусков программных блоков TideAnalise, PWSTR, HPW, PWWE, TideSTR
Путь к папке с данными
Номер первого дня в интервале, номер последнего дня в интервале
Номер обрабатываемого параметра
До семи широт через запятую
До четырёх высот через запятую

Пример файла:

19,30,72,37,24,2,5,5

1,1,1,1,1

D:\Data\New\_data\EAGLE-120

2,30

7

7,0,0,0,0,0,0

1,10,16,25

Первая строка файла info.txt содержит технические параметры файлов file4 и при обработке файлов от стабильной версии ГСМ ТИП с 19 записываемыми параметрами и сеткой 5 на 5 градусов по широтам и долготам не меняется.

Признаки запусков отдельных программных блоков предназначены для выборочного применения отдельных алгоритмов к ранее обработанным данным. Перечень необходимых входных данных и получаемых в результате запуска тех или иных блоков файлов приведён ниже. Если признак запуска имеет значение 1, соответствующий блок выполняется, во всех остальных случаях – нет.

Параметры распределены по номерам следующим образом:

1 – концентрация O2

2 – концентрация N2

3 – концентрация O

4 – концентрация NO

5 – концентрация N

6 – концентрация Mol+

7 – Температура нейтральных частиц, К

8 – Температура ионов, К

9 – Температура электронов, К

10 – Вертикальная скорость, см/с

11 – Меридиональная скорость, см/с

12 – Зональная скорость, см/с

13 – Скорость ионизации O2+, 1/см2/с

14 –  Скорость ионизации N2+, 1/см2/с

15 –  Скорость ионизации NO+, 1/см2/с

16 –  Скорость ионизации O+, 1/см2/с

17 –  Концентрация N(2D), см-3

18 –  

19 –

20 –

21 –

22 – Зональная компонента электрического поля, мВ/м

23 – Меридиональная компонента электрического поля, мВ/м

24 – TEC

### Структура программы

#### Блок GTF

Блок GTF предназначен для считывания параметров запуска и передачи их в блоки анализа в необходимой последовательности. Основная программа считывает info-файл и на основании полученной из него информации запускает блок TIDE или блок TIDE\_alt, а затем вызывает подпрограмму startpr.

Подпрограмма startpr отвечает за последовательный запуск блоков анализа приливных и планетарных волн в соответствии со считанными параметрами. Каждый из блоков запускается при условии наличия соответствующего признака запуска, при этом блок PWWE запускается до семи раз (для семи различных широт, заданных в info-файле), а блок TideSTR – до четырёх раз (для четырёх различных высот). Следует отметить, что в конечные блоки передаются только значения параметров, а не массивы, а потому при необходимости увеличения количества обрабатываемых широт или высот достаточно увеличить размерность соответствующего массива и добавить его элементы в блок чтения info-файла.

#### Блок TIDE

Блок предназначен для чтение файлов формата file4, перевода параметров из геомагнитной сетки в географическую, выделения приливных и планетарных вариаций и записи полученных результатов в отдельные файлы. Блок устроен таким образом, что все процедуры выполняются циклично, для каждого из дней заданного интервала. Строки 77-78 определяют выбор способа расположения файлов с данными: в папках по дням или пронумерованными в прямом порядке. Ненужный способ считывания следует держать закомментированным.

Ключевым является массив pgl, формируемый в ходе выполнения подпрограммы formPGL\_PGI. В него записываются все параметры, считываемые из файлов file4. Далее полученный массив пересчитывается из геомагнитных в географические координаты посредством подпрограммы filegeo.

Подпрограмма tides предназначена для формирования массивов данных по приливным волнам. Результатом её выполнения является трёхмерное распределение амплитуд и фаз приливов с периодами 24, 12 и 8 часов в каждом из параметров, записанное в пятимерных массивах (номер параметра, амплитуда или фаза, номер высотной, коширотной и долготной точки) tide24, tide12 и tide8 соответственно.

Подпрограмма means предназначена для формирования массивов данных по планетарным волнам. Её результатом является аналогичные распределения для планетарных волн, записанные в четырёхмерных массивах sutut, sutlt, sutj для всемирного, местного времени, а также выборки долгот соответственно. В массивы dsutut, dsutlt, dsutj записываются среднеквадратичные отклонения соответствующих параметров.

Созданные этими подпрограммами массивы дополняются значениями параметров на полюсах с помощью подпрограмм bongi и bongi2, и полученные результаты записываются в файлы с соответствующими именами.

#### Блок TIDE\_alt

Блок полностью аналогичен блоку TIDE и предназначен для чтения исходных данных о зональной и меридиональной компонентах электрического поля, а также TEC из результатов обработки с помощью программы GSMTIPViwer. Пути к данным и логика формирования из них стандартных для всего программного комплекса массивов обусловлены форматом генерируемых GSMTIPViwer данных и ориентированы исключительно на указанные параметры.

Блок TIDE\_alt запускается только в тех случаях, когда номер обрабатываемого параметра соответствует вышеуказанным параметрам. Для его корректной работы необходимо наличие в корневой папке с данными, путь к которой указан в info.txt вложенной папки 2009/01, в которой находятся папки, пронумерованные по дням месяца. В каждой из них должна находится вложенная папка quiet, включающая в себя исходные данные в файлах типа file4, а также автоматически генерируемые GSMTIPViwer подпапки с данными.

Для ускорения вычислений и ограничения размерности массивов, в указанной вложенной папке данные записываются в подпапки par01, par02 и par03 для 22, 23 и 24 параметров соответственно. Так как распределения компонент электрического поля и TEC понимаются двумерными, понятие фиксированной высоты не имеет смысла, а потому, для упрощения, в дальнейшем все данные пишутся для высоты 01.

#### Блок PWSTR

Блок предназначен для выделения из планетарной активности отдельных планетарных волн с различными зональными волновыми числами. Процедуры выполняются циклично для всех дней в заданном интервале. Основная программа считывает данные из файлов sutut, sutlt и sutj, после чего вызывается подпрограмма strucpw, в которой данные обрабатываются путём применения над ними дискретного преобразования Фурье. Полученные значения фонового состояния (нулевой гармоники) и первых пяти гармоник записываются в файлы PW0-n...PW5-n, где n – номер рассматриваемого дня.

#### Блок HPW

Блок предназначен для построения двумерных пространственных структур планетарных вариаций на фиксированной широте и фиксированной высоте. В силу технических ограничений в блоке создана условная подпрограмма HPW0, в которой фактически и происходит расчёт. После считывания данных из файлов PWi-n производится цикличный запуск подпрограмм PWH и PWZ для заданных в info-файле высот и широт соответственно. Каждая из них осуществляет выборочную запись данных в двумерные массивы, пригодные для визуализации.

#### Блок PWWE

Блок выделяет планетарные волны, бегущие в западном и восточном направлениях. Состоит из двух подпрограмм – технической RW1, предназначенной для чтения-записи данных и pwwestr, в которой осуществляется вычисление интеграла Фурье по номерам гармоник, с разделением результатов на восточную (для положительных номеров гармоник) и западную (для отрицательных номеров) волны. Полученные значения амплитуд и фаз волн записываются в файлы PEast.dat и PWest.dat соответственно.

#### Блок TideSTR

Блок TideSTR предназначен для аналогичной обработки приливных вариаций и включает в себя чтение исходных данных, вызов подпрограммы ppo, производящей преобразование Фурье над ними, и запись выходных данных. В результате исполнения блока формируются следующие файлы: par8.dat, par12.dat, par24.dat с амплитудами и фазами приливных вариаций с периодами 8, 12 и 24 часа соответственно, spec8.dat, spec12.dat, spec24.dat с разложением соответствующих волн по 11 гармоникам (от -5 до 5 включительно), а также общие файлы с распределением амплитуд и фаз приливов по Гринвичу PARut.dat, местному времени PARlt.dat, а также по широтам PARj.dat.

#### Запуск отдельных блоков

Следует заметить, что описанная выше процедура ввода параметров в полной мере актуальна только для произведения всего комплекса операций над исходными данными формата file4. В случае, если необходимо выполнить только определённые вычисления, возможен запуск программных блоков по отдельности.

Для запуска любого блока необходимо задать путь к каталогу с данными в вышеуказанном формате, интервал дней, включаемых в рассмотрение, а также выбрать параметр, данные по которому будут обрабатываться. Кроме этого, для корректной работы каждой из программ необходимо задать некоторые дополнительные параметры, а также убедиться в наличии входных данных нужного типа.

| Название | Параметры | Входные данные | Выходные данные |
| --- | --- | --- | --- |
| TideAnalise | =//= | file4 | sutj, sutlt, sutut, tide8, tide12, tide24 |
| PWSTR | =//= | sutj, sutlt, sutut, tide8, tide12, tide24 | PWn-d |
| HPW | широта, высоты 1,2,3,4 | PWn-d | PWnH, PWnh1, PWnh2, PWnh3, PWnh4, PWnh5 |
| PWWE | номер гармоники | PWn-d | PW0, PWest, PEast |
| Tidestr | высота | sutj, sutlt, sutut, tide8, tide12, tide24 | spec8-d, spec12-d, spec24-d, PAR8-d, PAR12-d, PAR24-d, PARj-d, PARj-d, PARlt-d |

здесь n – волновое число от 0 (для PWWE – 1) до 5, d – номера дней из заданного интервала.