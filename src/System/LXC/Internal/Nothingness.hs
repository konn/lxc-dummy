{-# LANGUAGE EmptyDataDecls, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE TemplateHaskell                                       #-}
module System.LXC.Internal.Nothingness where
import Control.Applicative ((<$>))
import Control.Monad       (forM)
import Foreign
import Foreign.C
import Language.Haskell.TH

data C'zfs_t = forall a. C'zfs_t a
data C'lvm_t = C'lvm_t CString CString (Ptr CChar)
data C'lxc_snapshot
data C_lxc_attach_exec_t
data C'lxc_attach_options_t
data C'lxc_attach_command_t
data C'lxc_container
data CBool
data C'bdev_specs = C'bdev_specs CString Word64 C'zfs_t C'lvm_t CString

data C'uint64_t
data C'pid_t

concat <$> (forM [ "c'LXC_ATTACH_LSM", "c'LXC_ATTACH_DEFAULT", "c'LXC_ATTACH_LSM_NOW"
                 , "c'LXC_ATTACH_REMOUNT_PROC_SYS", "c'LXC_ATTACH_LSM_EXEC"
                 , "c'LXC_ATTACH_SET_PERSONALITY", "p'lxc_attach_options_t'stderr_fd"
                 , "p'lxc_attach_options_t'stdout_fd", "p'lxc_attach_options_t'stdin_fd"
                 , "p'lxc_attach_options_t'extra_keep_env", "p'lxc_attach_options_t'extra_env_vars"
                 , "p'lxc_attach_options_t'env_policy", "p'lxc_attach_options_t'gid"
                 , "p'lxc_attach_options_t'uid", "p'lxc_attach_options_t'initial_cwd"
                 , "p'lxc_attach_options_t'personality", "p'lxc_attach_options_t'namespaces"
                 , "p'lxc_attach_options_t'attach_flags", "p'lxc_attach_command_t'argv"
                 , "p'lxc_attach_command_t'program", "p'lxc_attach_run_command"
                 , "p'lxc_attach_run_shell", "p'lxc_container'config_path"
                 , "c'LXC_CLONE_KEEPNAME", "c'LXC_CREATE_QUIET", "p'lxc_container'error_string"
                 , "c'LXC_CLONE_KEEPMACADDR", "c'LXC_CREATE_MAXFLAGS", "p'lxc_container'error_num"
                 , "c'LXC_CLONE_SNAPSHOT", "c'lxc_container_new", "p'lxc_container'is_defined"
                 , "c'LXC_CLONE_KEEPBDEVTYPE", "p'lxc_container'daemonize", "p'lxc_container'state"
                 , "c'LXC_CLONE_MAYBE_SNAPSHOT", "p'lxc_container'is_running"
                 , "p'lxc_container'config_file_name", "p'lxc_container'wait"
                 , "p'lxc_container'save_config", "p'lxc_container'rename"
                 , "p'lxc_container'set_config_item", "p'lxc_container'destroy"
                 , "p'lxc_container'want_daemonize", "p'lxc_container'want_close_all_fds"
                 , "p'lxc_container'load_config", "p'lxc_container'start", "p'lxc_container'stop"
                 , "c'LXC_CLONE_MAXFLAGS", "p'lxc_container'freeze", "p'lxc_container'init_pid"
                 , "p'lxc_container'reboot", "p'lxc_container'shutdown", "p'lxc_container'clear_config"
                 , "p'lxc_container'get_config_item", "p'lxc_container'get_running_config_item"
                 , "p'lxc_container'get_keys", "p'lxc_container'get_interfaces"
                 , "p'lxc_container'get_ips", "p'lxc_container'get_cgroup_item", "p'lxc_container'unfreeze"
                 , "p'lxc_container'set_cgroup_item", "p'lxc_container'clear_config_item"
                 , "p'lxc_container'get_config_path", "p'lxc_container'set_config_path"
                 , "p'lxc_container'clone", "p'lxc_container'console_getfd", "p'lxc_container'console"
                 , "p'lxc_container'attach", "p'lxc_container'attach_run_wait"
                 , "p'lxc_container'snapshot", "p'lxc_snapshot'name"
                 , "p'lxc_snapshot'comment_pathname", "p'lxc_snapshot'timestamp"
                 , "p'lxc_snapshot'lxcpath", "p'lxc_container'snapshot_list"
                 , "p'lxc_snapshot'free", "p'lxc_container'snapshot_restore"
                 , "p'lxc_container'snapshot_destroy", "p'lxc_container'may_control"
                 , "p'lxc_container'add_device_node", "p'lxc_container'remove_device_node"
                 , "p'lxc_container'create", "c'lxc_container_get"
                 , "c'lxc_container_put", "c'lxc_get_wait_states"
                 , "c'lxc_get_global_config_item", "c'lxc_get_version"
                 , "c'list_defined_containers", "c'list_active_containers"
                 , "c'list_all_containers", "c'lxc_log_close"] $ \name -> do
               sequence [sigD (mkName name) [t| forall a. a |], funD (mkName name) [clause [] (normalB [|error $(litE $ stringL name)|]) []]])
