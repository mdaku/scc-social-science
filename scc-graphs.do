clear
use "/Users/markdaku/Dropbox/Academic/Articles & Book Chapters/active/scc-social-science/SCC & Social Science/Stata/scc-dataset.dta"
cd "/Users/markdaku/Dropbox/Academic/Articles & Book Chapters/active/scc-social-science/SCC & Social Science/Stata/scc-graphs"

// Let's make some graphs, etc.

// Label this

la var case "Case ID"
la var legal "Legal Frame"
la var strategic "Strategic Frame"
la var legislative "Legislative Frame"
la var socialscience "Social Science Frame"
la var medical "Medical Frame"
la var decision "Pre/Post Decision"
la define decision_values 0 "Pre" 1 "Post"
la values decision decision_values
la var paper_code "Newspaper"


// We need to flip around the values of the cases, because they graph backwards
gen case_id =.
replace case_id = 0 if (case_name=="Rodriguez")
replace case_id = 1 if (case_name=="Carter")

drop case_name
rename case_id case_name
la var case_name "Case Name"
la define case_values 0 "Rodriguez" 1 "Carter"
la values case_name case_values


// Replace newspaper names
replace paper_code = "Financial Post" if (paper_code == "FIN")
replace paper_code = "Montreal Gazette" if (paper_code == "GAZ")
replace paper_code = "Globe and Mail" if (paper_code == "GLO")
replace paper_code = "Globe and Mail Breaking News" if (paper_code == "GMB")
replace paper_code = "Toronto Star" if (paper_code == "TOR")
replace paper_code = "Winnepeg Free Press" if (paper_code == "WFP")


// General change graph
graph bar (mean) legal (mean) strategic (mean) legislative (mean) socialscience (mean) medical, over(case_name)
graph save overall-graph, replace
graph export overall-graph.png, replace as(png) 

// By Newspaper
graph bar (mean) legal (mean) strategic (mean) legislative (mean) socialscience (mean) medical, over(paper_code) by(case_name)
graph save overall-by-newspaper, replace
graph export overall-by-newspaper.png, replace as(png)

graph bar (mean) legal (mean) strategic (mean) legislative (mean) socialscience (mean) medical, over(case_name) by(paper_code)
graph save overall-by-case-paper, replace
graph export overall-by-case-paper.png, replace as(png)


// Pre-post
graph bar (mean) legal (mean) strategic (mean) legislative (mean) socialscience (mean) medical, over(case_name) by(decision)
graph save overall-by-case-decision, replace
graph export overall-by-case-decision.png, replace as(png)

graph bar (mean) legal (mean) strategic (mean) legislative (mean) socialscience (mean) medical, over(decision) by(case_name)
graph save overall-by-decision-case, replace
graph export overall-by-decision-case.png, replace as(png)


// Summary Stats
ttest legal, by(case_name)
ttest strategic, by(case_name)
ttest legislative, by(case_name)
ttest socialscience, by(case_name)
ttest medical, by(case_name)

// More summary stats
ttest legal if (case_name=="Rodriguez"), by(decision)
ttest strategic if (case_name=="Rodriguez"), by(decision)
ttest legislative  if (case_name=="Rodriguez"), by(decision)
ttest socialscience if (case_name=="Rodriguez"), by(decision)
ttest medical if (case_name=="Rodriguez"), by(decision)

ttest legal if (case_name=="Carter"), by(decision)
ttest strategic if (case_name=="Carter"), by(decision)
ttest legislative  if (case_name=="Carter"), by(decision)
ttest socialscience if (case_name=="Carter"), by(decision)
ttest medical if (case_name=="Carter"), by(decision)
