module Main (main) where

import           Test.Hspec

import qualified Tox.Client.ChangesSpec
import qualified Tox.Client.DebugSpec
import qualified Tox.Client.ExportSpec
import qualified Tox.Client.FilesSpec
import qualified Tox.Client.FormatsSpec
import qualified Tox.Client.IdenticonSpec
import qualified Tox.Client.LinkedSpec
import qualified Tox.Client.LocationsSpec
import qualified Tox.Client.MessagesSpec
import qualified Tox.Client.NodesSpec
import qualified Tox.Client.QoiSpec
import qualified Tox.Client.RelaysSpec
import qualified Tox.Client.UpdateSpec

main :: IO ()
main = hspec $ do
  describe "Changes" Tox.Client.ChangesSpec.spec
  describe "Debug" Tox.Client.DebugSpec.spec
  describe "Export" Tox.Client.ExportSpec.spec
  describe "Files" Tox.Client.FilesSpec.spec
  describe "Formats" Tox.Client.FormatsSpec.spec
  describe "Identicon" Tox.Client.IdenticonSpec.spec
  describe "Linked" Tox.Client.LinkedSpec.spec
  describe "Locations" Tox.Client.LocationsSpec.spec
  describe "Messages" Tox.Client.MessagesSpec.spec
  describe "Nodes" Tox.Client.NodesSpec.spec
  describe "Qoi" Tox.Client.QoiSpec.spec
  describe "Relays" Tox.Client.RelaysSpec.spec
  describe "Update" Tox.Client.UpdateSpec.spec
