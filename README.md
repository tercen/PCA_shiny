# PCA_shiny_operator

##### Description

`PCA_shiny_operator` is an application that allows users to perform PCA on input data. 

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, y value per cell 

Input parameters|.
---|---
`Scale Spots`          | autoscale spots
`Number of Components` | Maximum number of components returned to Tercen
`Subtract component`   | Subtract component

Output|.
---|---
Loadings and scores for the first n principal components, where n is determined by the setting of the Number of Components property.

##### Details

The input is a cross-tab view with a single value per cell. Each column will be used as an observation (typically sample) for the PCA, each row as an variable (typically spot). A data color can be used to indicate a relevant grouping of the observations, this will be used for coloring in the score plots.

##### See Also

[PCA_shiny_operator](https://github.com/tercen/PCA_shiny_operator)
