{-# LANGUAGE EmptyDataDecls #-}
module System.LXC.Container (
  LXC,
  withContainer,
  -- * Data types
  Container(..),
  Snapshot(..),
  BDevSpecs(..),
  ContainerState(..),
  parseState, printState,
  -- * Flags
  CloneOption(..),
  CreateOption(..),
  cloneFlag, createFlag,
  -- * LXC errors
  LXCError(..),
  prettyLXCError,
  -- * Container methods
  -- ** Query container state.
  isDefined,
  isRunning,
  state,
  initPID,
  getInterfaces,
  getIPs,
  getDaemonize,
  getLastError,
  -- ** Container config
  configFileName,
  getConfigPath,
  setConfigPath,
  loadConfig,
  saveConfig,
  getKeys,
  setConfigItem,
  getConfigItem,
  getRunningConfigItem,
  clearConfig,
  clearConfigItem,
  -- ** Control container state
  start,
  stop,
  reboot,
  shutdown,
  freeze,
  unfreeze,
  wait,
  -- ** Manage containers
  create,
  clone,
  rename,
  destroy,
  -- ** Console
  consoleGetFD,
  console,
  -- ** Attach to container
  attach,
  attachRunWait,
  -- ** Snapshots
  snapshot,
  snapshotList,
  snapshotRestore,
  snapshotDestroy,
  -- ** Misc
  wantDaemonize,
  wantCloseAllFDs,
  getCGroupItem,
  setCGroupItem,
  mayControl,
  addDeviceNode,
  removeDeviceNode,
  -- * Global LXC functions
  -- ** List containers
  listDefinedContainers,
  listActiveContainers,
  listAllContainers,
  -- ** Misc
  getWaitStates,
  getGlobalConfigItem,
  getVersion,
  logClose,
  ) where
import System.LXC.Internal.Container
