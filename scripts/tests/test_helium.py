#!/usr/bin/env python3

import unittest
import sys
import os
import shutil
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open

import importlib.util

import importlib.util
import os

# Load helium module from ../path/helium
# Current: scripts/tests/test_helium.py
# Target: scripts/path/helium
current_dir = Path(__file__).parent.resolve()
helium_script_path = current_dir.parent / "path/helium"

if not helium_script_path.exists():
    raise FileNotFoundError(f"Helium script not found at {helium_script_path}")

import types

# Load helium module from ../path/helium
current_dir = Path(__file__).parent.resolve()
helium_script_path = current_dir.parent / "path/helium"

if not helium_script_path.exists():
    raise FileNotFoundError(f"Helium script not found at {helium_script_path}")

# Load module manually since it has no extension
helium = types.ModuleType("helium")
sys.modules["helium"] = helium
with open(helium_script_path, "r") as f:
    exec(f.read(), helium.__dict__)


class TestHeliumManager(unittest.TestCase):
    def setUp(self):
        # Create temp environment
        self.test_dir = tempfile.mkdtemp()
        self.helium_dir = Path(self.test_dir) / "opt/helium"
        self.binary = self.helium_dir / "chrome"
        self.profiles_dir = Path(self.test_dir) / "config/helium-profiles"
        self.log_file = Path(self.test_dir) / "local/share/helium.log"

        # Patch constants in helium module
        self.patches = [
            patch("helium.HELIUM_DIR", self.helium_dir),
            patch("helium.BINARY", self.binary),
            patch("helium.PROFILES_DIR", self.profiles_dir),
            patch("helium.LOG_FILE", self.log_file),
            patch("helium.VERSION_FILE", self.helium_dir / "VERSION"),
        ]
        for p in self.patches:
            p.start()

        # Create mock binary
        self.helium_dir.mkdir(parents=True)
        self.binary.touch()
        self.binary.chmod(0o755)

        # Create default profile
        (self.profiles_dir / "default").mkdir(parents=True)

    def tearDown(self):
        for p in self.patches:
            p.stop()
        shutil.rmtree(self.test_dir)

    def test_validate_installation_success(self):
        self.assertTrue(helium.HeliumManager.validate_installation())

    def test_validate_installation_missing_binary(self):
        self.binary.unlink()
        # Capture stderr to avoid clutter
        with patch("sys.stderr", new=MagicMock()):
            self.assertFalse(helium.HeliumManager.validate_installation())

    @patch("subprocess.Popen")
    def test_launch_default(self, mock_popen):
        args = MagicMock()
        args.profile = None
        args.incognito = False
        args.urls = []
        args.verbose = False
        args.fullscreen = False
        args.kiosk = False
        args.new_window = False
        args.no_sandbox = False
        args.disable_gpu = False
        args.remote_debugging_port = None

        helium.HeliumManager.launch(args)

        expected_cmd = [
            str(self.binary),
            f"--user-data-dir={self.profiles_dir}/default",
        ]
        mock_popen.assert_called_once()
        call_args = mock_popen.call_args[0][0]
        self.assertEqual(call_args, expected_cmd)

    @patch("subprocess.Popen")
    def test_launch_flags(self, mock_popen):
        args = MagicMock()
        args.profile = "testprof"
        args.incognito = True
        args.urls = ["example.com"]
        args.verbose = True
        args.fullscreen = True
        args.kiosk = True
        args.new_window = True
        args.no_sandbox = True
        args.disable_gpu = True
        args.remote_debugging_port = 9222

        # specific profile must exist
        (self.profiles_dir / "testprof").mkdir()

        helium.HeliumManager.launch(args)

        cmd = mock_popen.call_args[0][0]
        self.assertIn("--incognito", cmd)
        self.assertIn("--enable-logging", cmd)
        self.assertIn("--start-fullscreen", cmd)
        self.assertIn("--kiosk", cmd)
        self.assertIn("--new-window", cmd)
        self.assertIn("--no-sandbox", cmd)
        self.assertIn("--disable-gpu", cmd)
        self.assertIn("--remote-debugging-port=9222", cmd)
        self.assertIn(f"--user-data-dir={self.profiles_dir}/testprof", cmd)
        self.assertIn("example.com", cmd)

    @patch("subprocess.Popen")
    def test_launch_missing_profile(self, mock_popen):
        args = MagicMock()
        args.profile = "nonexistent"

        with patch("sys.stderr", new=MagicMock()):
            helium.HeliumManager.launch(args)

        mock_popen.assert_not_called()


class TestProfileManager(unittest.TestCase):
    def setUp(self):
        self.test_dir = tempfile.mkdtemp()
        self.profiles_dir = Path(self.test_dir)
        self.patch = patch("helium.PROFILES_DIR", self.profiles_dir)
        self.patch.start()

    def tearDown(self):
        self.patch.stop()
        shutil.rmtree(self.test_dir)

    def test_create_profile(self):
        helium.ProfileManager.create("work")
        self.assertTrue((self.profiles_dir / "work").exists())

    def test_delete_profile(self):
        (self.profiles_dir / "temp").mkdir()
        with patch("builtins.input", return_value="y"):
            helium.ProfileManager.delete("temp")
        self.assertFalse((self.profiles_dir / "temp").exists())

    def test_cannot_delete_default(self):
        (self.profiles_dir / "default").mkdir()
        helium.ProfileManager.delete("default")
        self.assertTrue((self.profiles_dir / "default").exists())


class TestMainIntegration(unittest.TestCase):
    def setUp(self):
        # We need to act as an integration test mocking sys.argv
        # but also patching everything since we don't want real execution
        self.test_dir = tempfile.mkdtemp()
        self.helium_dir = Path(self.test_dir) / "opt/helium"
        self.binary = self.helium_dir / "chrome"
        self.profiles_dir = Path(self.test_dir) / "config/helium-profiles"

        self.patches = [
            patch("helium.HELIUM_DIR", self.helium_dir),
            patch("helium.BINARY", self.binary),
            patch("helium.PROFILES_DIR", self.profiles_dir),
            patch("sys.stderr", new=MagicMock()),  # Silence errors
            patch("sys.stdout", new=MagicMock()),  # Silence output
            patch("subprocess.Popen"),
            patch("subprocess.run"),
        ]
        self.mocks = [p.start() for p in self.patches]
        self.mock_popen = self.mocks[5]

        # Setup env
        self.helium_dir.mkdir(parents=True)
        self.binary.touch()
        self.binary.chmod(0o755)
        (self.profiles_dir / "default").mkdir(parents=True)
        (self.profiles_dir / "xynapz").mkdir(parents=True)

    def tearDown(self):
        for p in self.patches:
            p.stop()
        shutil.rmtree(self.test_dir)

    def test_implicit_launch_profile(self):
        # Simulate `helium xynapz`
        with patch.object(sys, "argv", ["helium", "xynapz"]):
            helium.main()

        # Check if it behaved like `launch --profile xynapz`
        call_args = self.mock_popen.call_args[0][0]
        self.assertIn(f"--user-data-dir={self.profiles_dir}/xynapz", call_args)

    def test_implicit_launch_profile_does_not_exist(self):
        # Simulate `helium fake` -> should fail validation in launch
        # Assuming `launch` is called but aborts early
        with patch.object(sys, "argv", ["helium", "fake"]):
            helium.main()

        # Since profile 'fake' doesn't exist, launch returns early.
        # But wait, main() logic checks existence?
        # In main(): "If it exists in PROFILES_DIR... OR just treat as implicit launch request"
        # Since validation happens in launch(), it should try to launch and fail there.
        # Assert nothing launched
        self.mock_popen.assert_not_called()

    def test_launch_command(self):
        with patch.object(sys, "argv", ["helium", "launch", "http://example.com"]):
            helium.main()

        call_args = self.mock_popen.call_args[0][0]
        self.assertIn("http://example.com", call_args)
        self.assertIn(f"--user-data-dir={self.profiles_dir}/default", call_args)


class TestLockAndKill(unittest.TestCase):
    def setUp(self):
        self.test_dir = tempfile.mkdtemp()
        self.profiles_dir = Path(self.test_dir) / "config"
        self.helium_dir = Path(self.test_dir) / "opt"
        self.binary = self.helium_dir / "chrome"

        # Patches
        self.patches = [
            patch("helium.PROFILES_DIR", self.profiles_dir),
            patch("helium.HELIUM_DIR", self.helium_dir),
            patch("helium.BINARY", self.binary),
            patch("sys.stderr", new=MagicMock()),
            patch("sys.stdout", new=MagicMock()),
            patch("subprocess.run"),
        ]
        for p in self.patches:
            p.start()

        self.profiles_dir.mkdir(parents=True)
        self.helium_dir.mkdir(parents=True)
        self.binary.touch()

    def tearDown(self):
        for p in self.patches:
            p.stop()
        shutil.rmtree(self.test_dir)

    def test_check_stale_lock_removes_stale(self):
        # Create a profile
        p_dir = self.profiles_dir / "stale_prof"
        p_dir.mkdir()
        lock = p_dir / "SingletonLock"

        # Create a valid symlink structure: hostname-999999
        # assert 999999 is not running
        fake_pid = 999999
        os.symlink(f"myhost-{fake_pid}", lock)

        # Verify initial state
        self.assertTrue(os.path.lexists(lock))

        # Call check_stale_lock
        # We need to ensure /proc/999999 does not exist.
        # Typically it won't, but let's mock Path.exists just for the proc check?
        # The code does: Path(f"/proc/{pid}").exists()
        # It's hard to mock only THAT Path instance without side effects.
        # But 999999 is unlikely to exist.

        helium.HeliumManager.check_stale_lock(p_dir)

        # Should be removed
        self.assertFalse(os.path.lexists(lock))

    @patch("os.kill")
    def test_kill_specific_pid(self, mock_kill):
        # Setup profile with lock
        p_dir = self.profiles_dir / "running_prof"
        p_dir.mkdir()
        lock = p_dir / "SingletonLock"
        lock.touch()  # Just needs to exist for iterdir check

        # Mock is_symlink to return True (since touch creates regular file)
        # We can't easily mock pathlib.Path.is_symlink on a specific instance provided by iterdir.
        # So we actually make it a symlink.
        lock.unlink()
        os.symlink("host-12345", lock)

        # Run kill
        helium.HeliumManager.kill()

        # Check if os.kill was called with 12345
        mock_kill.assert_called_with(12345, helium.signal.SIGTERM)


if __name__ == "__main__":
    unittest.main()
