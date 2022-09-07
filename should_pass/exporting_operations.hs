MLM { name = Name "exporting_operations"
    , classes =
        [ Class
            { isAbstract = False
            , level = 1
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes = []
            , operations = [ Operation
                { level = 0
                , name = Name "myOperation"
                , dataType = Integer
                , isMonitored = True
                , body = "@Operation myOperation[monitor=true]():XCore::Integer&#10;  99&#10;end"
                }
            ]
            , slots = []
            }
        ]
    , associations = []
    , links = []
    }
