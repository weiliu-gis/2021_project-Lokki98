proposal
================
Ziqi Tang

# Introduction to problem/question

As Earth’s population grows, remote sensing of nighttime light emissions
offers a unique perspective for investigations into some of these human
behaviors. China’s economy has developed rapidly in recent years.
However, there are gaps in urban construction and economic development
in different regions.

### Problem / Question

What are the characteristics of the distribution and change of nighttime
value in China from 2000 to 2017?

# Inspiring Examples

## Example 1

<center>
<img src="https://raw.githubusercontent.com/GEO511-2021/2021_case_studies-Lokki98/master/map.png">
</center>

I found this graphic easy to understand the CO2 emissions in different
countries. This type of graphic could be used to reflect the
distribution of night light in each province of China.

## Example 2

<center>
<img src="https://www.visme.co/wp-content/uploads/2019/12/i_Monthly-Active-Users-of-an-App-Line-Graph_full.jpg">
</center>

I found this graphic easy to understand the changes in the numbers of
monthly active users in different countries. This type of graphic could
be used to reflect the changes in nighttime value in each province from
2000 to 2017.

## Example 3

<center>
<img src="https://www.nasa.gov/sites/default/files/thumbnails/image/2016-north-america-usa.jpg" width = "80%">
</center>

This composite image shows the continental U.S. at night. The
relationship between metropolitan agglomerations can be felt more
naturally on a raster map.

# Proposed data sources

[An extended time-series (2000-2018) of global NPP-VIIRS-like nighttime
light
data](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/YGIVCD)

<center>
<img src="https://raw.githubusercontent.com/GEO511-2021/2021_case_studies-Lokki98/master/nightlight.png">
</center>

# Proposed methods

1.read_xlsx: read data in Excel

2.filter&dplyr: select the maximum and mean value from the nighttime
data

3.ggplot: plot the nighttime value on the map

# Expected results

1.  The changes in the nighttime value of different provinces in China
    from 2000 to 2017

2.  The distribution of nighttime value in China
