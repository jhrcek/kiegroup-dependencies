module Maven.DepGraph.Report.IO
   ( readReport
   , allReports
   ) where
import Maven.DepGraph.Report (Report)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson as Aeson
allReports :: IO (Either String [Report])
allReports = sequence <$> mapM readReport reports

readReport :: FilePath -> IO (Either String Report)
readReport file = Aeson.eitherDecode <$> BS.readFile file


-- TODO replace with dynamic searching for dependency-graph.json files
reports :: [FilePath]
reports = fmap ("/home/jhrcek/Devel/github.com/kiegroup/" ++)
    [ "lienzo-tests/dependency-graph.json"
    , "kie-uberfire-extensions/dependency-graph.json"
    , "droolsjbpm-build-bootstrap/dependency-graph.json"
    , "kie-wb-common/dependency-graph.json"
    , "jbpm-designer/dependency-graph.json"
    , "jbpm-wb/dependency-graph.json"
    , "drools/dependency-graph.json"
    , "kie-wb-playground/dependency-graph.json"
    , "jbpm/dependency-graph.json"
    , "droolsjbpm-tools/dependency-graph.json"
    , "kie-docs/dependency-graph.json"
    , "kie-wb-distributions/dependency-graph.json"
    , "drlx-parser/dependency-graph.json"
    , "optaplanner/dependency-graph.json"
    , "droolsjbpm-integration/dependency-graph.json"
    , "optaplanner-wb/dependency-graph.json"
    , "appformer/dependency-graph.json"
    , "droolsjbpm-knowledge/dependency-graph.json"
    , "drools-wb/dependency-graph.json"
    , "kie-soup/dependency-graph.json"
    , "jbpm-work-items/dependency-graph.json"
    , "optashift-employee-rostering/dependency-graph.json"
    , "lienzo-core/dependency-graph.json"
    ]
