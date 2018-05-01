Set of tools to enable analysis of maven dependencies across multi-repo maven project

TODO
- [x] Collect results of `mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf` to a single file
- [x] Parse and validate resulting TGF reports (split groupId:artifactId:packaging[:qualifier]:version[:scope])
- [x] Figure out convenient representation of TFG data for easy consumption by frontend
- [x] For each artifact calculate number of direct/transitive dependencies
- [x] For each artifact calculate number of packages that directly/transitively depend on it
- [ ] Given GAV show all dependency paths in all trees that lead to it
- [ ] Given GAV show all maven modules that depend on it
- [ ] Distinguish between internal and 3rd party dependencies
- [ ] Visualize dependency trees for each maven module
- [ ] Visualize module structure for each repo
- [ ] Show conflicts (groupId:artifactId with different versions?)
- [ ] Include results of [dependency:analyze-report](https://maven.apache.org/plugins/maven-dependency-plugin/analyze-report-mojo.html)
