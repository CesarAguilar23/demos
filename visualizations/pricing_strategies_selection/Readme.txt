To view the html file, go to: https://rawcdn.githack.com/CesarAguilar23/demos/7667768dfb3d9d86eb498ae3b4be5f3fe509113d/visualizations/pricing_strategies_selection/index.html

Description
This visualization shows the results of a pricing optimization algorithm, that simulates relevant banking KPIs. The optimization is run with different parameters, shown in 
four different colors and a legend. This helps identify the parameter combination that reports the best strategies.
It has two sections, the first four graphs show scatterplots showing the relationship between two important variables. Each point shows a particular pricing strategy 
for the entire portfolio. The scatterplots enable us to visualize tradeoffs between variables such as net credit margin, losses, and loan volume, which aids in selecting
several of them for further analysis. The second section has line graphs representing the distribution of relevant banking KPIs by each APR "bucket" for the strategies, 
each being a line. This drill down enables the analyst to inspect the KPIs by pricing level, which aids in the determination of a correctly balanced strategy, in terms of
responders, loan volume, among others by APR. 
The visualization is interactive, allowing to select one (tap select tool) or a subset (lasso,poly, and box select tools) of strategies to focus on them. This "grays out" the
rest of the strategies, allowing for detailed inspection. More so, the selection is done across all graphs, enabling the selection of a relevant strategy in the first section,
and to inspect other aspects of it in the rest of the document. The visualization also includes a hover tool, that shows a summary of the main portfolio KPIs for that 
strategy at a glance.

Note: All work required for this visualization was independently conducted, data was randomly generated in a way that is adequate for the field.  
