# visual_air

**A visualization of air pollution in China**

## Motivation
Air pollution, especially PM2.5, has become an big problem in China. This project aims to visualize the air pollution over time and arouse people's awareness of its severity.

## Project Structure
The folder "houbao" contains a crawler to acquire daily air pollution data from tianqihoubao.com.

The folder "analysis" contains the R scripts to analyze and visualize air pollution.

The folder "maps" contains files to create a map of China

## Methodology
Spatial-temporal kriging is used to to obtain pollutant concentration level on a grid of 50000 equally spaced points. The model reflects the intuition that pollutant concentration should be similar if either the space or time is close.

While not being the state-of-the-art model in modeling air pollution, this model has an advantage in its simplicity and lucidity.