Set of tools to enable analysis of maven dependencies across multi-repo maven project

TODO
- [x] Collect results of `./mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf` to a single directory
- [x] Parse and validate resulting TGF reports (split groupId:artifactId:packaging[:qualifier]:version[:scope])
- [ ] Figure out convenient representation of TFG data for easy consumption by frontend
- [ ] Visualize dependency trees for each maven module
- [ ] Visualize module structure for each repo
- [ ] Given GAV show all dependency paths in all trees that lead to it
- [ ] Given GAV show all maven modules that depend on it
- [ ] Show conflicts (groupId:artifactId with different versions?)
- [ ] Distinguish between internal and 3rd party dependencies
- [ ] Include results of [dependency:analyze-report](https://maven.apache.org/plugins/maven-dependency-plugin/analyze-report-mojo.html)
