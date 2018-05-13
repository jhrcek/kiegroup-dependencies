# Kiegroup dependencies

Tools to enable analysis of maven dependencies across multi-repo maven project(s).

## TODOs
- [x] Collect results of `mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf` to a single file
- [x] Parse and validate resulting TGF reports (split groupId:artifactId:packaging[:qualifier]:version[:scope])
- [x] Come up with a convenient representation of TFG data for easy consumption by front end
- [x] For each artifact calculate number of direct/transitive dependencies
- [x] For each artifact calculate number of direct/transitive reverse dependencies
- [x] Show dependency tree of each artifact
- [x] Show reverse dependency tree of each artifact
- [x] Distinguish between internal and 3rd party dependencies (based on groupId)
- [x] Add breadcrumb showing "Home >> groupId >> artifactId >> version" for easier navigation
- [x] Show scope of each dependency in the dependency trees
- [x] Dependency convergence report (groupId:artifactId with multiple different versions)
- [ ] Visualize module structure for each repo
- [ ] Make dependency tree view have configurable depth
- [ ] Include results of [dependency:analyze-report](https://maven.apache.org/plugins/maven-dependency-plugin/analyze-report-mojo.html)
- [ ] Add link from each kiegroup artifact to it's associated github repository

## How to use it

1. clone all kiegroup repos
```
mkdir kiegroup
cd kiegroup
git clone git@github.com:kiegroup/droolsjbpm-build-bootstrap.git
./droolsjbpm-build-bootstrap/script/git-clone-others.sh
```

2. Run maven to produce dependency tree reports. This will produce several hundreds maven module dependency reports (1 per maven module).
```
./droolsjbpm-build-bootstrap/script/mvn-all.sh \
dependency:tree -DoutputType=tgf  -DoutputFile=deps.tgf  -DfullProfile
```

3. This tool uses Haskell script to collect and deduplicate all individual dependency trees into one huge dependency graph. You need to [install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) to run it.

4. Build the tool
```
make build
```

5. Run the tool to collect all dependency reports into one file. This will produce `dependency-graph.json` containing all dependency data.
```
make report
```

6. Build the website that enables you to browse the data in this model
```
make front
```

7. HTTP serve the `dist` directory and open file index.html in your browser
```
make run
```
