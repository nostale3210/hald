module Hald.Cli where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo,
    ReadM,
    argument,
    command,
    eitherReader,
    footer,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    strOption,
    switch,
    value,
  )
import Text.Read (readMaybe)

positiveInt :: ReadM Int
positiveInt = eitherReader $ \s ->
  case readMaybe s of
    Just n | n >= 0 -> Right n
    _ -> Left "Expected a non-negative integer"

data GlobalOpts = GlobalOpts
  { optRootdir :: !String,
    optSystemd :: !Bool,
    optCommand :: !Command
  }

data CasCommand = CasGc | CasFsverity

data Command
  = Dep
      { updateFlag :: Bool,
        buildFlag :: Bool,
        seFlag :: Bool,
        activateFlag :: Bool,
        gcFlag :: Bool,
        stateFlag :: Bool,
        secureBoot :: Bool,
        uki :: Bool,
        casFlag :: Bool,
        hardlinkFlag :: Bool
      }
  | Activate Int
  | Status Bool
  | Diff Int Int
  | Rm Int
  | Gc
  | Cas CasCommand

optsParser :: ParserInfo GlobalOpts
optsParser =
  info
    (helper <*> commandOptions)
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
    <*> switch
      ( long "skip-systemd-inhibit"
          <> help "Don't invoke systemd-inhibit even if available"
      )
    <*> hsubparser (depCommand <> activateCommand <> statusCommand <> diffCommand <> rmCommand <> gcCommand <> casCommand)

depCommand :: Mod CommandFields Command
depCommand =
  command "dep" (info depOptions (progDesc "Create deployments"))

depOptions :: Parser Command
depOptions =
  Dep
    <$> switch
      ( long "activate"
          <> short 'a'
          <> help "Activate new deployment immediately after creation"
      )
    <*> switch
      ( long "build"
          <> short 'b'
          <> help "Build custom containerfile before creating deployment"
      )
    <*> switch
      ( long "drop-state"
          <> short 'd'
          <> help "Only keep essential configuration (fstab, passwd,...)"
      )
    <*> switch
      ( long "gc"
          <> short 'g'
          <> help "Perform garbage collection after creating deployment"
      )
    <*> switch
      ( long "update"
          <> short 'u'
          <> help "Pull latest container image before creating deployment"
      )
    <*> switch
      ( long "relabel"
          <> short 'z'
          <> help "Relabel new deployment according to selinux contexts"
      )
    <*> switch
      ( long "sb"
          <> short 's'
          <> help "Sign the deployment's kernel with sbctl"
      )
    <*> switch
      ( long "uki"
          <> short 'k'
          <> help "Build UKI using ukify"
      )
    <*> switch
      ( long "cas"
          <> short 'c'
          <> help "Use content-addressable store (CAS) backend (no-op)"
      )
    <*> switch
      ( long "hardlink"
          <> short 'H'
          <> help "Use rsync hardlink backend"
      )

rmCommand :: Mod CommandFields Command
rmCommand =
  command "rm" (info rmOptions (progDesc "Delete deployments"))

rmOptions :: Parser Command
rmOptions =
  Rm <$> argument positiveInt (metavar "ID")

gcCommand :: Mod CommandFields Command
gcCommand =
  command "gc" (info (pure Gc) (progDesc "Perform garbage collection"))

activateCommand :: Mod CommandFields Command
activateCommand =
  command "activate" (info activateOptions (progDesc "Activate a deployment"))

activateOptions :: Parser Command
activateOptions =
  Activate <$> argument positiveInt (metavar "ID")

statusCommand :: Mod CommandFields Command
statusCommand =
  command "status" (info statusOptions (progDesc "Display all deployments and their metadata"))

statusOptions :: Parser Command
statusOptions =
  Status
    <$> switch
      ( long "compact"
          <> short 'c'
          <> help "Compact representation"
      )

diffCommand :: Mod CommandFields Command
diffCommand =
  command "diff" (info diffOptions (progDesc "Compare two deployments"))

diffOptions :: Parser Command
diffOptions =
  Diff
    <$> option positiveInt (long "from" <> short 'f' <> help "First deployment (optional)" <> value 0 <> metavar "ID")
    <*> option positiveInt (long "to" <> short 't' <> help "Second deployment (optional)" <> value 0 <> metavar "ID")

casCommand :: Mod CommandFields Command
casCommand =
  command "cas" (info (Cas <$> hsubparser (casGcCommand <> casFsverityCommand)) (progDesc "CAS operations"))

casGcCommand :: Mod CommandFields CasCommand
casGcCommand =
  command "gc" (info (pure CasGc) (progDesc "Perform garbage collection on the CAS"))

casFsverityCommand :: Mod CommandFields CasCommand
casFsverityCommand =
  command "fsverity" (info (pure CasFsverity) (progDesc "Enable fs-verity on all CAS objects"))
