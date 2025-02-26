module Main.Cli where

import Options.Applicative

data GlobalOpts = GlobalOpts
  { optRootdir :: !String,
    optCommand :: !Command
  }

data Command
  = Dep
      { updateFlag :: Bool,
        buildFlag :: Bool,
        seFlag :: Bool,
        activateFlag :: Bool,
        gcFlag :: Bool,
        stateFlag :: Bool
      }
  | Activate Int
  | Status
  | Rm Int
  | Gc

optsParser :: ParserInfo GlobalOpts
optsParser =
  info
    (commandOptions <**> helper)
    ( fullDesc
        <> progDesc "Create and manage deployments from containers"
        <> header "hald - somewhat functional atomic deployments"
        <> footer "Might erase all your data - glhf"
    )

commandOptions :: Parser GlobalOpts
commandOptions =
  GlobalOpts
    <$> strOption
      ( long "rootd"
          <> short 'r'
          <> metavar "ROOTDIR"
          <> value "/"
          <> help "Operate on a different root directory"
      )
    <*> hsubparser (depCommand <> activateCommand <> statusCommand <> rmCommand <> gcCommand)

depCommand :: Mod CommandFields Command
depCommand =
  command "dep" (info depOptions (progDesc "Create deployments"))

depOptions :: Parser Command
depOptions =
  Dep
    <$> flag
      False
      True
      ( long "activate"
          <> short 'a'
          <> help "Activate new deployment immediately after creation"
      )
    <*> flag
      False
      True
      ( long "build"
          <> short 'b'
          <> help "Build custom containerfile before creating deployment"
      )
    <*> flag
      False
      True
      ( long "dropState"
          <> short 'd'
          <> help "Only keep essential configuration (fstab, passwd,...)"
      )
    <*> flag
      False
      True
      ( long "gc"
          <> short 'g'
          <> help "Perform garbage collection after creating deployment"
      )
    <*> flag
      False
      True
      ( long "update"
          <> short 'u'
          <> help "Pull latest container image before creating deployment"
      )
    <*> flag
      False
      True
      ( long "relabel"
          <> short 'z'
          <> help "Relabel new deployment according to selinux contexts"
      )

rmCommand :: Mod CommandFields Command
rmCommand =
  command "rm" (info rmOptions (progDesc "Delete deployments"))

rmOptions :: Parser Command
rmOptions =
  Rm <$> argument auto (metavar "ID")

gcCommand :: Mod CommandFields Command
gcCommand =
  command "gc" (info (pure Gc) (progDesc "Perform garbage collection"))

activateCommand :: Mod CommandFields Command
activateCommand =
  command "activate" (info activateOptions (progDesc "Activate a deployment"))

activateOptions :: Parser Command
activateOptions =
  Activate <$> argument auto (metavar "ID")

statusCommand :: Mod CommandFields Command
statusCommand =
  command "status" (info (pure Status) (progDesc "Display all deployments and their metadata"))
