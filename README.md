# applicative-settings

This library creates functionality for reading setting files in an
hierarchical manner, where different files may be read by different
parsers and all the information is recovered with an applicative
interface inspired by optoparse-applicative.

##How to use

Define your settings structure and constructor, and create a parser in
applicative style. Read your settings files and other sources, and
evaluate the settings into your structure:

    import ApSettings

    data MySettings = MySettings {aSetting :: String, anotherSetting :: [Double], yetAnother :: Int}

    readSettings = do
        let settingsParser = MySettings <$>
            onKey "aSetting" `scalar` text <*>
            onKey "anotherSetting" `multiple` value real <*>
            onKey "yetAnother" `scalar` integral
        setts <- mapM readYaml ["file1", "file2", "file3"]
        pure $ evaluateSettings settingsParser setts

    main = do
        s' <- readSettings
        case s' of
	    Left e -> hPutStr stderr e
	    Right s -> do some stuff with your (s :: MySettings)

