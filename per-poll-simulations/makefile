polls := \
	0010-reid-research \
	0016-colmar-brunton \
	0023-reid-research \
	0024-colmar-brunton \
	0037-colmar-brunton \
	0045-reid-research \
	0060-colmar-brunton \
	0060-reid-research \
	0082-colmar-brunton \
	0110-reid-research \
	0110-roy-morgan \
	0117-colmar-brunton \
	0138-roy-morgan \
	0167-roy-morgan \
	0187-colmar-brunton \
	0193-reid-research \
	0202-roy-morgan \
	0222-colmar-brunton \
	0230-roy-morgan \
	0257-roy-morgan \
	0293-roy-morgan \
	0309-colmar-brunton \
	0320-roy-morgan \
	0342-roy-morgan \

latest := $(shell ls  | head -n1)

.PHONY: all-polls latest

latest: $(latest)/Simulated-PV.csv

all-polls: $(addsuffix /error.txt,$(polls))

%/Simulated-PV.csv: common/Election-Simulation-post-ME-Polls.R %/config.R %/PollsForGam.csv
	cd $* && Rscript ../$^

%/error.txt : common/Election-Simulation-post-ME-Polls.R %/config.R %/PollsForGam.csv
	cd $* && Rscript ../$^ 2>$@

