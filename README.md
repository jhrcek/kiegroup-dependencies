# POMs Cleanup

Tools to enable analysis of maven dependencies across multi-repo maven project(s).

## TODOs
- [x] Collect results of `mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf` to a single file
- [x] Parse and validate resulting TGF reports (split groupId:artifactId:packaging[:qualifier]:version[:scope])
- [x] Come up with a convenient representation of TFG data for easy consumption by front end
- [x] For each artifact calculate number of direct/transitive dependencies
- [x] For each artifact calculate number of direct/transitive reverse dependencies
- [x] Visualize dependency tree for each artifact
- [x] Given artifact's GAV, show all its dependencies
- [x] Given artifact's GAV, show all its reverse dependencies
- [x] Distinguish between internal and 3rd party dependencies
- [ ] Add breadcrumb showing "Home >> groupId >> artifactId >> version" for easier navigation
- [ ] Show scope of each dependency in the dep. trees
- [ ] Visualize module structure for each repo
- [ ] Make dependency tree view have configurable depth
- [ ] Show conflicts (groupId:artifactId with different versions?)
- [ ] Include results of [dependency:analyze-report](https://maven.apache.org/plugins/maven-dependency-plugin/analyze-report-mojo.html)

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
./droolsjbpm-build-bootstrap/script/mvn-all.sh dependency:tree -DoutputType=tgf  -DoutputFile=deps.tgf  -DfullProfile
```

3. [install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

4. Build this tool

```
make build
```

5. Run the tool to collect all dependency reports into one file. This will produce `dependency-graph.json` containing all dependency data.

```
cd frontend/collect-deps/dist
stack exec collect-deps -- /PATH/TO/kiegroup # where you cloned all the repos in step 1
```


6. Build the website that enables you to browse the data in this model

```
make front
```

7. HTTP serve the `dist` directory and open file index.html in your browser
```
make run
```
