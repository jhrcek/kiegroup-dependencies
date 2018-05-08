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
- [ ] Visualize module structure for each repo
- [ ] Make dependency tree view have configurable depth
- [ ] Show conflicts (groupId:artifactId with different versions?)
- [ ] Include results of [dependency:analyze-report](https://maven.apache.org/plugins/maven-dependency-plugin/analyze-report-mojo.html)
