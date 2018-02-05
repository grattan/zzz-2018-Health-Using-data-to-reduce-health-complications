library(grattanCharts)
library(ggplot2)
library(data.table)
library(magrittr)
library(showtext)


pdf("boxplot.pdf", width = 8.864, height = 9.5)
showtext_begin()
helvet_path <- file.path("~/sandbox", "fonts")

font_add("helvet",
         regular = file.path(helvet_path, "uhvr8a.pfb"),
         bold = file.path(helvet_path, "uhvb8a.pfb"),
         italic = file.path(helvet_path, "uhvro8a.pfb"))

fread("Fluid management	Nutrition support	Complications with childbirth	Return to theatre or procedure room	Haemorrhage/haematoma management	Ventilatory support	Other complications	Nervous system complications	Metabolic disorders	Haematological diorders	Perinatal complications	Labour and delivery complications	Early pregnancy complications	Hospital-acquired psychiatric states	Genitourinary complications	Skin conditions	Gastrointestinal complications	Resipratory complications	Cardiovascular complications	Hospital-acquired infections	Accidental injuries	Adverse drug events	Procedural complications	Any CHADx+	Any HAC
0	0.00484	0	0.02712	0.04704	0.05528	0.02687	0.00124	0.00337	0.000582	0	0.00287	0	0.00536	0.00401	0.00715	0.01543	0.0055	0.01693	0.00883	0.000424	0.00116	0.02182	2.67046	0.04134
      0.00209	0.02901	0.00247	0.05695	0.38175	0.29524	0.60392	0.03733	0.22285	0.099328	0.00738	0.04145	0	0.15156	0.21908	0.19348	0.40207	0.19755	0.58351	0.46839	0.100736	0.16333	0.36276	4.12022	0.60711
      0.00825	0.03463	1.34402	0.05885	0.46704	0.64912	0.77009	0.07642	0.59033	0.28646	0.03262	2.32618	0.00341	0.33209	0.50152	0.32463	0.74922	0.32028	0.87525	0.57555	0.17847	0.20082	0.70006	3.82316	0.83388
      0.03052	0.06819	1.02755	0.10789	0.54286	0.7669	0.8576	0.09626	1.12781	0.45274	0.12907	1.99309	0.01035	0.36763	0.4698	0.36571	0.64627	0.52122	1.21407	0.66115	0.17139	0.34661	0.87536	4.13493	1.12971
      0.06403	0.09098	0.67433	0.0971	0.78397	0.42229	0.92743	0.16912	1.42181	0.36847	0.086	1.25356	0.01287	0.51268	0.66055	0.41335	0.69794	0.42639	0.98157	0.81608	0.26164	0.38001	0.69865	3.55953	1.05062
      ", sep = "\t") %>%
  setnames("Haemorrhage/haematoma management", "Haemorrhage/haematoma mgmt.") %>%
  .[, I := c("y0", "y25", "y50", "y75", "y100")] %>%
  melt.data.table(id.vars = "I") %>%
  .[, value := cumsum(value), keyby = "variable"] %>%
  dcast.data.table(variable ~ I, value.var = "value") %>%
  .[, variable := factor(variable, levels = unique(.$variable), ordered = TRUE)] %>%
  .[, width := 0.6] %>%
  grplot(aes(x = variable,
             ymin = y0, 
             lower = y25,
             middle = y50,
             upper = y75,
             ymax = y100,
             width = width,
             fill = variable != "Any HAC")) + 
  geom_boxplot(stat = "identity") +
  theme_hugh(base_family = "helvet", base_size = 18) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        axis.line.x = element_line(size = 0.5)) +
  scale_y_continuous(expand = c(0.025, 0.025)) +
  coord_flip()
showtext_end()
dev.off()





