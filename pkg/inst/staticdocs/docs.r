library(staticdocs)
library(grid)
list(
    readme = "",
    
    index = list(
        sd_section("Training and Analysis functions",
            "These functions are used for training and analysis",
            c(
                "sPipeline",
                "sHexGrid",
                "sTopology",
                "sInitial",
                "sTrainology",
                "sTrainSeq",
                "sTrainBatch",
                "sBMH",
                "sNeighDirect",
                "sNeighAny",
                "sHexDist",
                "sDistance",
                "sDmat",
                "sDmatMinima",
                "sDmatCluster",
                "sCompReorder",
                "sWriteData",
                "sMapOverlay"
            )
        ),
        sd_section("Visualisation functions",
            "These functions are used for visualisation",
            c(
                "visHexPattern",
                "visHexGrid",
                "visHexMapping", 
                "visHexComp",
                "visColormap",
                "visColoralpha",
                "visColorbar",
                "visVp",
                "visHexMulComp",
                "visHexAnimate",
                "visCompReorder", 
                "visDmatCluster",
                "visDmatHeatmap",
                "visKernels",
                "visHeatmap",
                "visHeatmapAdv",
                "visTreeBootstrap",
                "visTreeBSclust"
            )
        ),
        sd_section("Built-in datasets",
            "",
            c(
                "Fang",
                "Golub", 
                "Xiang"
            )
        )

    ),
    
    if(0){
    icons = list(  
        sPipeline = sd_icon({
          textGrob("Common", rot = 45, gp = gpar(cex = 1))
        }),
        visHexPattern = sd_icon({
          textGrob("Hot", rot = 45, gp = gpar(cex = 1.2))
        }),
        sDmatCluster = sd_icon(inherit = "sPipeline"),
        sCompReorder = sd_icon(inherit = "sPipeline"),
        sWriteData = sd_icon(inherit = "sPipeline"),
        sMapOverlay = sd_icon(inherit = "sPipeline"),
        visHexMulComp = sd_icon(inherit = "visHexPattern"),
        visCompReorder = sd_icon(inherit = "visHexPattern"),
        visDmatCluster = sd_icon(inherit = "visHexPattern")
    )
    }
)
