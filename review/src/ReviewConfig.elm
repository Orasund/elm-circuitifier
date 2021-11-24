module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import NoAlways
import ReviewPipelineStyles.Premade
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes

config : List Rule
config =
    [ NoAlways.rule
    , ( [ ReviewPipelineStyles.Premade.noMultilineLeftComposition 
        , ReviewPipelineStyles.Premade.noPipelinesWithSimpleInputs
        , ReviewPipelineStyles.Premade.noRepeatedParentheticalApplication
        , ReviewPipelineStyles.Premade.noMultilineLeftPizza
        ] 
      ) |> List.concat |> ReviewPipelineStyles.rule
    ]
