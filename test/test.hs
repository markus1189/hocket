module Main (main) where

import Core.AgentTests (agentProtocolTests, agentSnapshotTests)
import Core.BatchTests (batchTests)
import Core.CharacterizationTests (characterizationTests)
import Core.ClassifyTests (classifyTests)
import Core.FilterTests (filterStateTests, filterTuningTests, fuzzyFilterMatchTests, fuzzyMatchTests, sanitizeForDisplayTests)
import Core.IntentTests (intentTests)
import Core.JsonTests (dateTimeParsingTests, jsonRoundtripTests, raindropApiBodyTests, raindropParsingTests)
import Core.KeyIntentTests (keyIntentTests)
import Core.NavTests (navTests)
import Core.PlanTests (planTests)
import Core.Properties (propertyTests)
import Core.ReminderTests (reminderTests)
import Core.StateTests (hocketStateTests, xKeyBatchConcurrencyTests)
import Core.UpdateTests (updateTests)
import Core.VisibleTests (visibleTests)
import Shell.AgentParallelTests (agentParallelTests)
import Shell.AgentServerTests (agentServerIntegrationTests)
import Shell.FakeTests (fakeTests)
import Shell.FixtureTests (fixtureTests)
import Shell.HeadlessTests (headlessTests)
import Shell.SMCoreTests (smCoreTests)
import Shell.SMFixtureTests (smFixtureTests)
import Shell.SimulateTests (simulateTests)
import Shell.SnapshotGoldenTests (snapshotGoldenTests)
import Shell.SystemTests (systemTests)
import Shell.WorkerTests (workerTests)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)

main :: IO ()
-- Every test, a whole property with its shrinks included, gets two minutes
-- (the suite takes about ten seconds): a regression that makes something
-- wait forever fails instead of hanging the run.
main = defaultMain (localOption (mkTimeout (120 * 1000000)) tests)

-- One group per line, so parallel branches adding groups do not conflict.
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ smCoreTests,
      smFixtureTests,
      simulateTests,
      agentParallelTests,
      snapshotGoldenTests,
      xKeyBatchConcurrencyTests,
      propertyTests,
      visibleTests,
      navTests,
      intentTests,
      keyIntentTests,
      planTests,
      systemTests,
      fakeTests,
      fixtureTests,
      reminderTests,
      hocketStateTests,
      raindropParsingTests,
      raindropApiBodyTests,
      classifyTests,
      characterizationTests,
      updateTests,
      workerTests,
      headlessTests,
      batchTests,
      dateTimeParsingTests,
      jsonRoundtripTests,
      sanitizeForDisplayTests,
      fuzzyMatchTests,
      fuzzyFilterMatchTests,
      filterStateTests,
      filterTuningTests,
      agentSnapshotTests,
      agentProtocolTests,
      agentServerIntegrationTests
    ]
