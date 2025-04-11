###############################################
#     server part
#
#
###############################################
# source("data/packages.r")

	server <- function(input, output, session) {
	  
	#overall:
	  # definition of some table headers, if there is a double-row table header needed, or if there should be additional information available with mouse hover
	  sketchcadd = htmltools::withTags(table(
		class = 'display',
		
		thead( # start of table header
		  tr( # first table header row
			th('RSID'), # first cell
			th(class="PPA hover", 'PPA', p(class="PPAcomment", "Posterior probability of association of the SNP (probability of driving the association of the respective signal)")), # cell with additional information
			th('Affected gene'),
			th('Locus ID'),
			th(class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			th('Functional consequence'),
			th(class="REF hover",'Reference allele', span(class="REFcomment", "Allele for reference aminoacid")),
			th(class="ALT hover",'Alternative allele', span(class="ALTcomment", "Allele for altered aminoacid")),
			th('Aminoacid position'),
			th('Reference aminoacid'),
			th('Alternative aminoacid'),
			th(class="CADDscore hover",'CADD Phred-Score',span(class="CADDscorecomment", "Scaled score for the deleteriousness of this allelic exchange"))
		  )
		)
	  ))
	  
	  sketchcredvar = htmltools::withTags(table(
		class = 'display',
		
		thead(
		  tr(
			th( rowspan=2, 'RSID*'), # cell spans two rows
			th(rowspan=2,'Locus ID'),
			th(rowspan=2,class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			th(rowspan=2,class="PPA hover", 'PPA', p(class="PPAcomment", "Posterior probability of association of the SNP (probability of driving the association of the respective signal)")),
			th(rowspan=2,'N'),
			th(rowspan=2,'Effect allele'),
			th(rowspan=2,'Other allele'),
			th(rowspan=2,'EAF'),
			th(colspan=3, 'Association with log(eGFRcrea) conditioned on other signal index variants'), # cell spans three columns
			th(colspan=3, 'Association with log(eGFRcrea) unconditioned'),
			th(rowspan=2,'Chr'),
			th(rowspan=2, class="pos hover",'Pos',span(class="poscomment","SNP position from GRCh37")),
		  ),
		  tr( # second row of this header
			lapply(c('beta', 'StdErr', 'P-value','beta', 'StdErr', 'P-value'), th)
		  )
		)
	  ))
	  
	  sketchcredvargene = htmltools::withTags(table(
		class = 'display',
		
		thead(
		  tr(
			th( rowspan=2, 'RSID*'),
			th(rowspan=2,'Locus ID'),
			th(rowspan=2,class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			th(rowspan=2,class="PPA hover", 'PPA', p(class="PPAcomment", "Posterior probability of association of the SNP (probability of driving the association of the respective signal)")),
			th(rowspan=2, 'Functional impact on the searched gene'),
			th(rowspan=2,'N'),
			th(rowspan=2,'Effect allele'),
			th(rowspan=2,'Other allele'),
			th(rowspan=2,'EAF'),
			th(colspan=3, 'Association with log(eGFRcrea) conditioned on other signal index variants'),
			th(colspan=3, 'Association with log(eGFRcrea) unconditioned'),
			th(rowspan=2,'Chr'),
			th(rowspan=2, class="pos hover",'Pos',span(class="poscomment","SNP position from GRCh37")),
		  ),
		  tr(
			lapply(c('beta', 'StdErr', 'P-value','beta', 'StdErr', 'P-value'), th)
		  )
		)
	  ))
	  
	  sketchdmstatgene = htmltools::withTags(table(
		class = 'display',
		
		thead(
		  tr(
			th(rowspan=2, 'RSID'),
			th(rowspan=2,'Effect allele'),
			th(rowspan=2,'Other allele'),
			th(colspan=5, 'Association of eGFRcrea in individuals with Diabetes'),
			th(colspan=5, 'Association of eGFRcrea in individuals without Diabetes'),
			th(rowspan=2,'P-Value of the difference in association between Diabetes and no Diabetes')
		  ),
		  tr(
			lapply(c('EAF', 'beta', 'StdErr','P-value', 'N', 'EAF', 'beta', 'StdErr','P-value', 'N'), th)
			
		  )
		)
	  ))
	  
	  sketchallsig = htmltools::withTags(table(
		class = 'display',
		
		thead(
		  tr(
			th( rowspan=2, 'RSID*'),
			th(rowspan=2,'Chr'),
			th(rowspan=2,'Pos'),
			th(rowspan=2,'Effect allele'),
			th(rowspan=2,'Other allele'),
			th(rowspan=2,'N'),
			th(rowspan=2,'EAF'),
			th(colspan=3,'Association with log(eGFRcrea) unconditioned**'),
			th(rowspan=2,'Nearest gene'),
			th(rowspan=2,'Distance to nearest gene'),
			th(rowspan=2,'Locus ID')
		  ),
		  tr(
			lapply(c('beta', 'StdErr', 'P-value'), th)
		  )
		)
	  ))

	# GPS ---------------------------------------------------------------------
	  # GPS tab
	  
	  ## header of GPS table
	  sketch = htmltools::withTags(table(  
		class = 'display',
		
		thead(
		  tr(
			th(rowspan = 2, class="Geneofficial hover", 'Gene*', span(class="Genecomment", "Official gene name from HGNC")),
			th(rowspan = 2, 'Locus name**'),
			th(rowspan = 2, class="Locusidwithcomment hover",'Locus ID', span(class="Locusidcomment", "k: known locus from Wuttke et al. Nat.commun.2019, n: novel locus in Stanzick et al. Nat.commun. 2021")),
			th(rowspan = 2, class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			th(rowspan = 2, class="validationwithcomment hover",'eGFRcys or BUN validation', span(class="validationcomment",list( "Information whether the locus lead variant is nominal significant (P<0.05) associated with concordent effect direction with ", tags$abbr(title="glomerular filtration rate estimated from serum cystatin C","eGFRcys"),"or ",tags$abbr(title="blood urea nitrogen","BUN")))),
			th(rowspan = 2, class="DMwithcomment hover",'Signal association depends on DM', span(class="DMcomment",list( "Information whether the signal index variant (or a correlated variant) shows significant interaction with ", tags$abbr(title="Diabetes mellitus","DM"), "-status, Winkler et al. 2022"))),
			th(rowspan = 2, class="declinewithcomment hover",'Signal association with eGFRcrea decline', span(class="declinecomment",list( "Information whether the signal index variant (or a correlated variant) was established with eGFR-decline in Gorski et al. 2022 or Wiegrebe et al. 2024"))),
			th(rowspan = 2, '# credible variants in signal'),
			th(colspan = 3, class="CADD", span('# protein-relevant credible variants in the gene')),
			th(colspan = 6, class="eqtl", '# credible variants that modulate gene expression (eQTLs) or splicing (sQTLs), FDR < 5%'),
			th(rowspan = 2, class="eqtl", 'Coloc in NEPTUNE tissue'),
			th(rowspan = 2, class="pheno",'# kidney phenotypes in mouse'),
			th(rowspan = 2, class="pheno", '# kidney phenotypes in human'),
			th(rowspan = 2, class ="drug", 'Gene is known as drug target or for drug interaction'),
			th(rowspan = 2, 'Distance to locus lead variant'),
			th(rowspan = 2, 'Chromosome'),
			th(rowspan = 2, 'Position gene start'),
			th(rowspan = 2, 'Position gene end')
		  ),
		  tr(
			th('stop-gained, stop-lost, non-synonymous'),
			th('canonical splice, noncoding change, synonymous, splice site'),
			th(span('other deleterious variant (CADD-Phred',HTML('<span>&#8805;</span>'),'15)')),
			th(class='eQTLglowithcomment hover', 'eQTL glomerulus', span(class='eQTLglocomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			th(class='eQTLtubwithcomment hover','eQTL tubulo-interstitium', span(class='eQTLtubcomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			th('eQTL kidney cortex (GTEx)'),
			th('eQTL other tissue (GTEx)'),
			th('sQTL kidney cortex (GTEx)'),
			th('sQTL other tissue (GTEx)')
		  )
		)
	  ))
	  
	  # color definition of header:
	  headerCallback <- "function( thead, data, start, end, display ) {
	  $(thead).closest('thead').find('th').eq(8).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(9).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(10).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(11).css('background-color', '#b3ffb3');
	  $(thead).closest('thead').find('th').eq(12).css('background-color', '#b3ffb3');
	  $(thead).closest('thead').find('th').eq(13).css('background-color', '#d9b3ff');
	  $(thead).closest('thead').find('th').eq(18).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(19).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(20).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(21).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(22).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(23).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(24).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(25).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(26).css('background-color', '#ffdb99');
	  }"
	  
	  initComplete <- "function(settings, json) {
													$('.dt-button.buttons-collection').css('width', 'auto');
													$('.dt-button.buttons-collection select').css('width', '300px');
													$('.dt-button.buttons-collection').css('min-width', '300px');
												  }"
	  
	  
	  # render active radio buttons when input$signalppafilter is selected, else disable buttons
	  observe({
		if(input$signalppafilter){
		  output$signalppafilterdetail <- renderUI(
			radioButtons(inputId = "signalhigh", 
						 choiceNames = c("PPA >99% (most likely causal variants)", "PPA >50% (variants with high probability of being causal)", "PPA >10% (excluding variants with little probability of being causal)"),
						 choiceValues = c(0.99, 0.5, 0.1),
						 label =NULL,
						 selected = 0.5)
		  )
		}else{
		  output$signalppafilterdetail <- renderUI(
			disabled(radioButtons(inputId = "signalhigh", 
						 choiceNames = c("PPA >99% (most likely causal variants)", "PPA >50% (variants with high probability of being causal)", "PPA >10% (excluding variants with little probability of being causal)"),
						 choiceValues = c(0.99, 0.5, 0.1),
						 label =NULL,
						 selected = 0.5))
		  )
		}
		
		
	  })
	  
	  # update no signalnofilter button when input$signalppafilter changes
	  observeEvent(input$signalppafilter,{
		if(!(input$signalppafilter | input$signalsmall)){
		  updateCheckboxInput(session, "signalnofilter", value=TRUE, label = "no filter (show all signals)") # selected because the other option are not selected
		}
		req(input$signalppafilter)
		updateCheckboxInput(session, "signalnofilter", value=FALSE, label = "no filter (show all signals)") # not selected because input$signalppafilter is selected
	  })
	  
	  # update no signalnofilter button when input$signalsmall changes
	  observeEvent(input$signalsmall,{
		if(!(input$signalppafilter | input$signalsmall)){
		  updateCheckboxInput(session, "signalnofilter", value=TRUE, label = "no filter (show all signals)")
		}
		req(input$signalsmall)
		updateCheckboxInput(session, "signalnofilter", value=FALSE, label = "no filter (show all signals)")
	  })
	  
	  # update input$signalsmall and input$signalppafilter to FALSE when input$signalnofilter is selected
	  observeEvent(input$signalnofilter,{
		req(input$signalnofilter)
		updateCheckboxInput(session, "signalppafilter", value=FALSE, label = "The signal's credible set contains a variant with posterior probability of association (PPA) of:")
		updateCheckboxInput(session, "signalsmall", value=FALSE, label = "signal has a small credible set (1-5 variants)")
	  })
	  
	 # enable/disable detailed mapping options in dependence of the selection of protein or expression regulating variant: 
	observe({
		if (isTruthy(input$genemapprot) | isTruthy(input$genemapeqtl)) { # check if something is selected
		  updateCheckboxInput(session, "mapnofilter", label = "no filter (show all genes in selected signals)", value = FALSE) # update input$mapnofilter to FALSE because there is some mapping criterium selected
		  output$detail.genemap.ppa <- renderUI( # render additional options:
				  div(h4("Restrict mapping to credible set variants with any of the following properties regarding strength of statistical support:"),
				  checkboxInput(inputId = "mapppafilter", label ="Posterior probability of association (PPA):", value = TRUE),
									   div(id="gpsmapppafilter", 
										   uiOutput("mapppafilterdetail") # this is renderd below in dependence of input$mapppafilter
				  ),

				  checkboxInput(inputId = "mapsmall", label ="variant is contained in small credible set (1-5 variants)", value = TRUE),
				  div((checkboxInput(inputId = "nofiltermapppa", label =p("no filter (show all genes mapped by any credible set variant)"), value = FALSE)))
				  
				  )
		  )
				
		} else {
		  updateCheckboxInput(session, "mapnofilter", label = "no filter (show all genes in selected signals)", value = TRUE)
			  output$detail.genemap.ppa <- renderUI(
				div(h4("Restrict mapping to credible set variants with any of the following properties regarding strength of statistical support:"),
					disabled(checkboxInput(inputId = "mapppafilter", label ="Posterior probability of association (PPA):", value = FALSE)),
					div(id="gpsmapppafilter", 
						uiOutput("mapppafilterdetail")
					),
					
					disabled(checkboxInput(inputId = "mapsmall", label ="variant is contained in small credible set (1-5 variants)", value = FALSE)),
					div(disabled(checkboxInput(inputId = "nofiltermapppa", label =p("no filter (show all genes mapped by any credible set variant)"), value = TRUE)))
				)
			  )
		  
		}
	  })
	  
	#render mapppafilterdetail:
	observe({
	  output$mapppafilterdetail <- renderUI( # first render disabled. When input$mapppafilter is disabled it has no value, thus it can not be used as if-criterion
		disabled(radioButtons(inputId = "maphigh", 
							  choiceNames = c("PPA >99%", "PPA >50%", "PPA >10%"),
							  choiceValues = c(0.99, 0.5, 0.1),
							  label =NULL,
							  selected = 0.5))
	  )
	  req(input$mapppafilter) # check if input$mapppafilter has a value
	  if(input$mapppafilter){ #if input$mapppafilter is TRUE render radio buttons
		output$mapppafilterdetail <- renderUI(
		  radioButtons(inputId = "maphigh", 
					   choiceNames = c("PPA >99%", "PPA >50%", "PPA >10%"),
					   choiceValues = c(0.99, 0.5, 0.1),
					   label =NULL,
					   selected = 0.5)
		)
		
	  }else{ # else disable buttons
		output$mapppafilterdetail <- renderUI( 
		  disabled(radioButtons(inputId = "maphigh", 
								choiceNames = c("PPA >99%", "PPA >50%", "PPA >10%"),
								choiceValues = c(0.99, 0.5, 0.1),
								label =NULL,
								selected = 0.5 ))
		)
	  } 
	})

	observeEvent(input$mapnofilter,{ # update other options when input$mapnofilter is selected
	  req(input$mapnofilter)
	  updateCheckboxGroupInput(session,"genemapprot", 
							   selected = NULL,
							   choiceNames = c("stop-gained, stop-lost, non-synonymus","canonical splice, noncoding change, synonymous, splice site","other functional consequence with high predicted deleteriousness (CADD-Phred \u2265 15)"),
							   choiceValues = c(11:13)
							   
	  )
	  updateCheckboxGroupInput(session, "genemapeqtl", 
							   selected = NULL,
							   choiceNames = c("eQTL in glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in kidney cortex tissue (GTEx)","eQTL in other tissue (GTEx)","sQTL in kidney cortex tissue (GTEx)","sQTL in other tissue (GTEx)"), 
							   choiceValues = c(14:19)
	  )
	})

	observeEvent(input$nofiltermapppa,{ # update other options when input$nofiltermapppa is selected
	  if(input$nofiltermapppa){
	  updateCheckboxInput(session,"mapppafilter", 
							   value = FALSE
							   
	  )
	  updateCheckboxInput(session, "mapsmall", 
							   value=FALSE
	  )}
	})

	observe({ # update input$nofiltermapppa to FALSE when one of the other options is selected
	  
	  if(isTruthy(input$mapsmall)| isTruthy(input$mapppafilter)){
		updateCheckboxInput(session,
							"nofiltermapppa",
							value=FALSE)
	  }else{
		updateCheckboxInput(session,
							"nofiltermapppa",
							value=TRUE)
	  }
	  
	})

	observe({ # if nothing is selected set input$genelistnofilter to TRUE
	  if (!isTruthy(input$genelistpheno) && !isTruthy(input$genelistdrug)) {
		updateCheckboxInput(session, "genelistnofilter", value = TRUE)
	  } else {
		updateCheckboxInput(session, "genelistnofilter", value = FALSE)
	  }
	})

	observeEvent(input$genelistpheno, { # update input$genelistnofilter to FALSE when input$genelistpheno is selected
	  if (isTruthy(input$genelistpheno)) {
		updateCheckboxInput(session, "genelistnofilter", value = FALSE)
	  }
	})

	observeEvent(input$genelistdrug, {# update input$genelistnofilter to FALSE when input$genelistdrug is selected
	  if ( isTruthy(input$genelistdrug)) {
		updateCheckboxInput(session, "genelistnofilter", value = FALSE)
	  } 
	})

	observe({ #update the other options when input$genelistnofilter is selected
	  if (input$genelistnofilter) {
		updateCheckboxGroupInput(session,
								 "genelistpheno",
								 choiceNames = c("Gene has known kidney phenotypes in mouse models (Mouse Genome Informatics, MGI)","Gene is described to cause a human disease with kidney phenotype (online mendelian inheritance in man, OMIM, Groopman et al. [N.Engl.J.Med,2019], or Wopperer et al. [Kidney Int.,2022])"),
								 choiceValues = c("mouse","human"), 
								 selected = NULL)
		updateCheckboxGroupInput(session,
								 "genelistdrug",
								 choiceNames = c("Gene is a known drug target for kidney diseases (Therapeutic Drug Database)","Gene is a known drug target for any other diseases or with unspecific indication (Therapeutic Drug Database)"),
								 choiceValues = c("kidney", "other"),
								 selected = NULL)
	  }
	})


	   ##### check if all required options are selected:

	  go <- reactiveValues("gps_correct" = FALSE #go$gps_correct is FALSE as long as the options are not correctly selected
		
	  )
	  
	  observeEvent(input$gpsgo,{
		# check all wrong selectable options and let go$gps_correct at FALSE if something is not correct
		if((!input$signalppafilter&!input$signalsmall&!input$signalnofilter)|(!isTruthy(input$genemapprot)&!isTruthy(input$genemapeqtl)&!input$mapnofilter)|(!isTruthy(input$genelistpheno)&!isTruthy(input$genelistdrug)&!input$genelistnofilter)){ 
		  go$gps_correct = FALSE
		}else{ # else set to correct
		  go$gps_correct = TRUE
		}
	  })

	  observeEvent(input$gpsgo,{ # when the "Go Priotize" Button is clicked do the following
		if(!go$gps_correct){ # when the filter options are not correct
		showModal(modalDialog( # Show additional window with warning message
		  titel="Missing GPS filter-options",
		  tags$h4("Error: There are options missing for gene-prioritisation!", style="color: red;"),
		  p("Please check your selections. Did you select 'no filter' in section 1, 2 and 3 if you do not want to restrict your search results to any of the available options?"),
		  easyClose = TRUE, # one can close the window by clicking outside the window
		  footer=NULL,
		  size = "m" # size of the window
		))
		  }
	  })
	  
	  
	 
	  gps_gps_rows <- reactiveValues("signalfilter" = c(1:nrow(GPS)), "mapfilter"=c(1:nrow(GPS)), "genelistfilter"=c(1:nrow(GPS))) # introduce three reactive values which contain indices of the GPS Table. This will be reduced to those meeting the filter criteria in the following.
	  
	  observeEvent(input$gpsgo,{ # always waiting for the "Go Priotize"-Button
		if(go$gps_correct){ # and only doing something when the filter options are correct
		  
		  if(isTruthy(input$signalppafilter) & isTruthy(input$signalsmall)){ #PPA Filter AND small-set filter are selected
			gps_gps_rows$signalfilter=which(GPS$max_PPA>=input$signalhigh | GPS$credible_variants_per_locus_signal<=5) # reduce the indices to those meeting the signal filter criteria (high PPA OR small set)
		  }else{
			if(isTruthy(input$signalppafilter) & !isTruthy(input$signalsmall)){ # only PPA filter
			  gps_gps_rows$signalfilter=which(GPS$max_PPA>=input$signalhigh)
			}else if(!isTruthy(input$signalppafilter) & isTruthy(input$signalsmall)){ # only small-set filter
			  gps_gps_rows$signalfilter=which(GPS$credible_variants_per_locus_signal<=5)
			}else if(!isTruthy(input$signalppafilter) & !isTruthy(input$signalsmall)){ # no filter
			  gps_gps_rows$signalfilter=c(1:nrow(GPS))
			}
		  }
		  
		  ## restrict bun/cys signals:
		  if(input$signalcysbun==TRUE){
			gps_gps_rows$signalfilter=intersect(gps_gps_rows$signalfilter,which(GPS$Cys.BUN%in%c("bun","cys","cys/bun")))
		  }
		  ## restrict DM signals:
		  if(input$signalDM==TRUE){
			gps_gps_rows$signalfilter=intersect(gps_gps_rows$signalfilter,which(GPS$DM_effect!="-"))
		  }
		  ## restrict decline signals:
		  if(input$signaldecline==TRUE){
			gps_gps_rows$signalfilter=intersect(gps_gps_rows$signalfilter,which(GPS$decline_effect!="-"))
		  }
		}
	  })
	  
	  #reduce all GPS versions to the rows meeting the signal-filter criteria using the above defined indices
	  gps_version_all_show <- eventReactive(input$gpsgo,{
		GPS_show[gps_gps_rows$signalfilter,]
	  })
	  
	  gps_version_10_show <- eventReactive(input$gpsgo,{
		GPS_10_show[gps_gps_rows$signalfilter,]
	  })
	  
	  gps_version_50_show <- eventReactive(input$gpsgo,{
		GPS_50_show[gps_gps_rows$signalfilter,]
	  })
	  
	  
	# map filter: combine all mapping filter options to one vector
	  map <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(isTruthy(input$genemapprot)& isTruthy(input$genemapeqtl)){
			c(input$genemapprot,input$genemapeqtl)
		  }else if(isTruthy(input$genemapprot)& !isTruthy(input$genemapeqtl)){
			c(input$genemapprot)
		  }else if(!isTruthy(input$genemapprot)& isTruthy(input$genemapeqtl)){
			c(input$genemapeqtl)
		  }else if(isTruthy(input$mapnofilter)){
			 NULL
		  }
		  
		}
	  })
	  
	  
	  # combine the additional mapping filter options  (PPA, small set) to one variable
	  ppa_filter_gps <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(!is.null(map())){
			if(isTruthy(input$nofiltermapppa)){
			  "no_filter"
			}else{
			  if(isTruthy(input$mapppafilter)&isTruthy(input$mapsmall)){
				paste("small_", input$maphigh, sep="")
			  }else{
				if(isTruthy(input$mapppafilter)&!isTruthy(input$mapsmall)){
				  input$maphigh
				  
				}else if(!isTruthy(input$mapppafilter)&isTruthy(input$mapsmall)){
				  "small"
				}
			  }
			}
		  }
		}
	  })
	  
	  
	  #apply mapping filter to each GPS version in dependence of the additional mapping filter "ppa_filter_gps()" 
	  
	  # the full GPS version is needed for small signals and single signals (PPA>99%)
	  gps_version_all_show_map <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(!is.null(map())){
			if(any(ppa_filter_gps()%in%c(0.5, 0.1))){
			  NULL
			}else{
			  if(any(ppa_filter_gps()%in%c("small", "small_0.99", "small_0.5","small_0.1"))){
				if(length(map())==1){
				  gps_version_all_show()[which(gps_version_all_show()[,as.numeric(map())]!=0 & gps_version_all_show()[,10] <=5),]
				}else{
				gps_version_all_show()[which(rowSums(gps_version_all_show()[,as.numeric(map())])!=0 & gps_version_all_show()[,10] <=5),]
				}
			  }else if(ppa_filter_gps()=="no_filter"){
				if(length(map())==1){
				  gps_version_all_show()[which(gps_version_all_show()[,as.numeric(map())]!=0 ),]
				}else{
				  gps_version_all_show()[which(rowSums(gps_version_all_show()[,as.numeric(map())])!=0 ),]
				}
				
			  }else if(ppa_filter_gps()==0.99){
				if(length(map())==1){
				  gps_version_all_show()[which(gps_version_all_show()[,as.numeric(map())]!=0 & gps_version_all_show()[,10] ==1),]
				}else{
				  gps_version_all_show()[which(rowSums(gps_version_all_show()[,as.numeric(map())])!=0 & gps_version_all_show()[,10] ==1),]
				}
				
			  }
			}
		  }else{
			gps_version_all_show()
		  }
		}
	  })
	  
	  #PPA 10 version is needed for all mapping including a PPA >10% filtering (only PPA>10% and PPA>10% OR small set)
	  gps_version_10_show_map <- eventReactive(input$gpsgo,{
		if(go$gps_correct) {
		  if(!is.null(map())){
			if(ppa_filter_gps()%in%c("no_filter", "small_0.5", "small","small_0.99", 0.99,0.5)){
			  NULL
			}else{ #small_0.1 or 0.1
			  if(length(map())==1){
				gps_version_10_show()[which(gps_version_10_show()[,as.numeric(map())]!=0),]
			  }else{
				gps_version_10_show()[which(rowSums(gps_version_10_show()[,as.numeric(map())])!=0),]
			  }
			}
		  }else{
			NULL
		  }
		}
	  })
	  
	  #PPA 50 version is needed for all mapping including a PPA >50% filtering (only PPA>50% and PPA>50% OR small set)
	  gps_version_50_show_map <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(!is.null(map())){
			if(ppa_filter_gps()%in%c("no_filter", "small_0.1", "small","small_0.99", 0.99, 0.1)){
			 NULL
			}else{
			  #small_0.5 or 0.5
			  if(length(map())==1){
				gps_version_50_show()[which(gps_version_50_show()[,as.numeric(map())]!=0),]
			  }else{
				gps_version_50_show()[which(rowSums(gps_version_50_show()[,as.numeric(map())])!=0),]
			  }      
			}
		  }else{
			NULL
		  }
		}
	  })
	  
	  # combine gps_versions
	  
	  gps_version_mapped_show <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(is.null(gps_version_10_show_map())&is.null(gps_version_50_show_map())){ #no high ppa (10,50), all or small/single
			gps_version_all_show_map()
		  }else if(is.null(gps_version_10_show_map())&is.null(gps_version_all_show_map())){ # only ppa 50
			gps_version_50_show_map()
		  }else if(is.null(gps_version_all_show_map())&is.null(gps_version_50_show_map())){ #only high 10
			gps_version_10_show_map()
		  }else if(!is.null(gps_version_all_show_map()) & !is.null(gps_version_50_show_map())){ #small + ppa 50
			GPS_tbl = rbind(gps_version_all_show_map(), gps_version_50_show_map() ) # first entry always from small
			if(any(duplicated(GPS_tbl[,c("Gene*","Signal ID")]))){
			  GPS_tbl[which(!duplicated(GPS_tbl[,c("Gene*","Signal ID")])),] #remove potential second entries from high ppa
			}
		  }else if(!is.null(gps_version_all_show_map()) & !is.null(gps_version_10_show_map())){ #small + ppa 10
			GPS_tbl = rbind(gps_version_all_show_map(), gps_version_10_show_map() ) # first entry always from small
			if(any(duplicated(GPS_tbl[,c("Gene*","Signal ID")]))){
			  GPS_tbl[which(!duplicated(GPS_tbl[,c("Gene*","Signal ID")])),] #remove potential second entries from high ppa
			}
		  }
		}
		
	  })
	  

	  # output$GPS_datadescription <- renderText({ #option to test if filter options work properly
	  #   nrow(rbind(gps_version_all_show_map(), gps_version_50_show_map()))
	  # })
	  
	  ## pheno and drug filtering based on the before filtered and combined GPS versions
	  
	  gps_version_mapped_pheno_show <- eventReactive(input$gpsgo,{
		if(go$gps_correct){
		  if(!isTruthy(input$genelistpheno) & !isTruthy(input$genelistdrug)){
			gps_version_mapped_show()
		  }else{
			pheno = NULL
			if(isTruthy(input$genelistpheno)){
			  if(length(input$genelistpheno)==2){
				pheno = c(20,21)
			  }else{
				pheno = ifelse(input$genelistpheno=="mouse",20,21)
			  }
			}
			drug=NULL
			if(isTruthy(input$genelistdrug)){
			  if(length(input$genelistdrug)==2){
				drug = c("yes for kidney disease", "yes for other disease")
			  }else{
				drug = ifelse(input$genelistdrug=="kidney", "yes for kidney disease", "yes for other disease")
			  }
			  
			}
			if(is.null(pheno)){
			  gps_version_mapped_show()[which(gps_version_mapped_show()[,"known_drug_target"] %in% drug),]
			}else if(is.null(drug)){
			  if(length(pheno)==1){
				gps_version_mapped_show()[which(gps_version_mapped_show()[,pheno]!=0),]
			  }else{
				gps_version_mapped_show()[which(rowSums(gps_version_mapped_show()[,pheno])!=0),]
			  }
			  
			}else if(!is.null(pheno) & ! is.null(drug)){
			  if(length(pheno)==1){
				gps_version_mapped_show()[which(gps_version_mapped_show()[,pheno]!=0 | gps_version_mapped_show()[,"known_drug_target"] %in% drug),]
			  }else{
				gps_version_mapped_show()[which(rowSums(gps_version_mapped_show()[,pheno])!=0 | gps_version_mapped_show()[,"known_drug_target"] %in% drug),]
			  }
			  
			}
		  }
		}
	  })
	  

	  
	  go_correct <- reactiveVal(FALSE) # additional reactive value. Introduced to handle the shown GPS version before "Go Prioritize" was clicked 
	  
	  observeEvent(input$gpsgo, { # when "Go Prioritize" was clicked 
		if(go$gps_correct){ # and filter criteria are ok
		  go_correct(TRUE) #this changes to TRUE
		}
		
	  })
	  
	  
	  
	  gps_show_final <- reactive({
		if (go_correct()) {
		  gps_version_mapped_pheno_show()
		} else {
		  GPS_show # as long as go_correct() is FALSE the whole GPS is shown
		}
	  })
	  
	  
	  output$downloadGPSTable <- downloadHandler(
		filename = function() {
		  paste("results_KidneyGPS_", Sys.Date(), ".xlsx", sep = "")
		},
		content = function(file) {
		  data_to_export <- gps_show_final()[,GPS_columns] # are defined below, but this does not matter. It is a fixed vector used to order the GPS columns
		  data_to_export[,"Gene*"] <- gsub('<a href=".*">|</a>', '', data_to_export[,"Gene*"]) # excludes link from GPS Genes
		  write_xlsx(data_to_export, file)
		}
	  )
	 
	  
	  ### GPS_show column names
	  # [1] "Locus name**"
	  # [2] "Locus ID"
	  # [3] "Signal ID"
	  # [4] "Gene*"
	  # [5] "Distance to locus lead variant"
	  # [6] "Chromosome"
	  # [7] "Position gene start"
	  # [8] "Position gene end"
	  # [9] "eGFRcys or BUN validation"
	  # [10] "# credible variants in signal"
	  # [11] "stop-gained, stop-lost, non-synonymus"
	  # [12] "canonical splice, noncoding change, synonymous, splice site"
	  # [13] "other deleterious variant"
	  # [14] "eQTL glomerulus (NEPTUNE, or Sheng et al [Nat Genet, 2021])"
	  # [15] "eQTL tubulo-interstitium (NEPTUNE, or Sheng et al [Nat Genet, 2021])"
	  # [16] "eQTL kidney cortex (GTEx)"
	  # [17] "eQTL other tissue (GTEx)"
	  # [18] "sQTL kidney cortex (GTEx)"
	  # [19] "sQTL other tissue (GTEx)"
	  # [20] "# kidney phenotypes in mouse"
	  # [21] "# kidney phenotypes in human"
	  # [22] "Coloc in NEPTUNE tissue"
	  # [23] "DM_effect"
	  # [24] "decline_effect"
	  # [25] "known_drug_target"
	  # [26] "Score"
	  # [27] "max PPA"
	  GPS_columns <- match(c('Gene*','Locus name**','Locus ID','Signal ID','eGFRcys or BUN validation','DM_effect','decline_effect','# credible variants in signal','stop-gained, stop-lost, non-synonymus','canonical splice, noncoding change, synonymous, splice site','other deleterious variant','eQTL glomerulus (NEPTUNE, or Sheng et al [Nat Genet, 2021])','eQTL tubulo-interstitium (NEPTUNE, or Sheng et al [Nat Genet, 2021])','eQTL kidney cortex (GTEx)','eQTL other tissue (GTEx)','sQTL kidney cortex (GTEx)','sQTL other tissue (GTEx)','Coloc in NEPTUNE tissue','# kidney phenotypes in mouse','# kidney phenotypes in human','known_drug_target','Distance to locus lead variant','Chromosome','Position gene start','Position gene end'),names(GPS_show))
	  
	 
	 
		 output$GPS <- renderDataTable({ # THis renders the shown GPS Table
		  datatable(gps_show_final()[,GPS_columns],
					rownames = FALSE,
					options = list(
					  columnDefs = list(list(className = 'dt-left', targets = "_all")),
					  initComplete=JS(initComplete),
					  headerCallback = JS(headerCallback), # define header
					  dom = 'rltip' # Define the table control elements to appear on the page and in what order. r: pRocessing display element, l: length changing menu, t: table, i: Table Information summary, P: pagination
					  ),
					
					class ='display',
					filter = list(
					  position = 'top', clear = TRUE, plain = TRUE # filter fields above the table columns
					),
					escape = FALSE,

					caption = htmltools::tags$caption( # text displayed below the table
					  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
					  '* Link to GeneCards, **nearest gene to locus lead-variant'),
					container = sketch
					)
		 })

	  
	  output$GPS_Genes1 <- renderText({ # counts number of genes and signals in the displayed GPS Table
		ngene_unique <- length(unique(gps_show_final()[,'Gene*']))
		nsignal <- length(unique(paste(gps_show_final()[,'Locus ID'], gps_show_final()[,'Signal ID'],sep = ".")))
		paste(ngene_unique, "genes from",nsignal, "signals meet the applied filter criteria.")
	  })
	 

	  
	  
	  
	  observeEvent(input$question_GPS,{ # Window when the ? is clicked
		showModal(modalDialog(
		  titel="Help GPS filter-options",
		  tags$h4("GPS filter-options - Help:"),
		  p("This tab allows to generate customized gene lists. Select filter options from all three sections and click 'Go prioritze' to generate your gene list. When all 5906 genes in all 594 signals should be shown, the 'no filter' options have to be selected."),
		  easyClose = TRUE,
		  footer=NULL,
		  size = "m"
		))
	  })
								 

	# region page -------------------------------------------------------------
	  
	  sketch2 = htmltools::withTags(table(
		class = 'display',
		
		thead(
		  tr(
			th(rowspan = 2, class="Geneofficial hover", 'Gene*', span(class="Genecomment", "Official gene name from HGNC")),
			th(rowspan = 2, 'Locus name**'),
			th(rowspan = 2, class="Locusidwithcomment hover",'Locus ID', span(class="Locusidcomment", "k: known locus from Wuttke et al. Nat.commun.2019, n: novel locus in Stanzick et al. Nat.commun. 2021")),
			th(rowspan = 2, class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			th(rowspan = 2, class="validationwithcomment hover",'eGFRcys or BUN validation', span(class="validationcomment",list( "Information whether the locus lead variant is nominal significant (P<0.05) associated with concordent effect direction with ", tags$abbr(title="glomerular filtration rate estimated from serum cystatin C","eGFRcys"),"or ",tags$abbr(title="blood urea nitrogen","BUN")))),
			th(rowspan = 2, class="DMwithcomment hover",'Signal association depends on DM', span(class="DMcomment",list( "Information whether the signal index variant (or a correlated variant) shows different effect on eGFR in individuals with ", tags$abbr(title="Diabetes mellitus","DM"), "-status, Winkler et al. 2022"))),
			th(rowspan = 2, class="declinewithcomment hover",'Signal association with eGFRcrea decline', span(class="declinecomment",list( "Information whether the signal index variant (or a correlated variant) was established with eGFR-decline in Gorski et al. 2022 or Wiegrebe et al. 2024"))),
			th(rowspan = 2, '# credible variants in signal'),
			th(rowspan = 2, class="maxPPA hover",'max PPA', span(class="maxPPAcomment", "max. probability of a credible variant in this signal to be causal")),
			th(colspan = 3, class="CADD", span('# protein-relevant credible variants in the gene')),
			th(colspan = 6, class="eqtl", '# credible variants that modulate gene expression (eQTLs) or splicing (sQTLs), FDR < 5%'),
			th(rowspan = 2, class="eqtl", 'Coloc in NEPTUNE tissue'),
			th(rowspan = 2, class="pheno",'# kidney phenotypes in mouse'),
			th(rowspan = 2, class="pheno", '# kidney phenotypes in human'),
			th(rowspan = 2, class ="drug", 'Gene is known as drug target or for drug interaction'),
			th(rowspan = 2, 'Distance to locus lead variant'),
			th(rowspan = 2, 'Chromosome'),
			th(rowspan = 2, 'Position gene start'),
			th(rowspan = 2, 'Position gene end')
		  ),
		  tr(
			th('stop-gained, stop-lost, non-synonymous'),
			th('canonical splice, noncoding change, synonymous, splice site'),
			th(span('other deleterious variant (CADD-Phred',HTML('<span>&#8805;</span>'),'15)')),
			th(class='eQTLglowithcomment hover', 'eQTL glomerulus', span(class='eQTLglocomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			th(class='eQTLtubwithcomment hover','eQTL tubulo-interstitium', span(class='eQTLtubcomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			th('eQTL kidney cortex (GTEx)'),
			th('eQTL other tissue (GTEx)'),
			th('sQTL kidney cortex (GTEx)'),
			th('sQTL other tissue (GTEx)')
		  )
		)
	  ))
	  

	  headerCallback2 <- "function( thead, data, start, end, display ) {
	  $(thead).closest('thead').find('th').eq(9).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(10).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(11).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(12).css('background-color', '#b3ffb3');
	  $(thead).closest('thead').find('th').eq(13).css('background-color', '#b3ffb3');
	  $(thead).closest('thead').find('th').eq(14).css('background-color', '#d9b3ff');
	  $(thead).closest('thead').find('th').eq(19).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(20).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(21).css('background-color', '#ccffff');
	  $(thead).closest('thead').find('th').eq(22).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(23).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(24).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(25).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(26).css('background-color', '#ffdb99');
	  $(thead).closest('thead').find('th').eq(27).css('background-color', '#ffdb99');
	  }"
	  
	  
	  #region page
	  observe(
		if(input$pos_start!=0&input$pos_end!=0){ # only when there is input show the output section
		  shinyjs::showElement(id="region_go_div")
		}
	  )
	  
	  #combine ENTER and "go" click to one reactive value only reacting if there is region input
	  go <- reactiveValues('region'=c())
	  
	  observeEvent(input$region_go, {
		if(input$pos_start!=0&input$pos_end!=0){
		  if(is.null(go$region)){go$region=1} 
		  else{go$region <- go$region+1} #count +1 when the go button is clicked
		  
		}
	  })
	  observeEvent(input$keyPressed, {
		if(input$pos_start!=0&input$pos_end!=0){
		  if(is.null(go$region)){go$region=1}
		  else{go$region <- go$region+3} #count +3 when the when enter is clicked
		}
	  })
	  
	  region_start <- eventReactive(go$region,{input$pos_start}) # value with the start position
	  region_end <- eventReactive(go$region,{input$pos_end}) # value with the end position
	  
	  valid_reg <- eventReactive( go$region,{( # check if a valid input is made
		if(!is.na(region_start())& !is.na(region_end())){(is.integer(region_start()) & is.integer(region_end())& input$pos_start>0 & input$pos_end>0 & input$pos_end>input$pos_start & input$pos_start!=input$pos_end)} # when every criterium is met this will be TRUE
		else{FALSE}
		
	  )}
	  )
	  
	  chr <- eventReactive(go$region,{input$Chromosome})
	  
	  output$valid_gene_test <- renderText({
		if(!valid_reg()){
		  if(is.na(region_start())){"Error: invalid region start position"}
		  else{
			if(!is.integer(region_start())|region_start()<0) {"Error: invalid region start position"}
			else{
			  if(is.na(region_end())){"Error: invalid region end position"}
			  else{
				if(!is.integer(region_end())| region_end()<0) {"Error: invalid region end position"}
				else{
				  if(region_end()<region_start()) {"Error: region start must be smaller than region end position"}
				  else{
					if(region_end()==region_start()){
					  "Error: region start and end must not be the same position"
					}else{"undefined error"}
				  }
				}
			  }
			}
		  }
		}
	  })
	  
	  
	  ov_loci<- reactive({ #search for overlapping loci
		if(valid_reg()){
		  
		  region_table_show[which(chr()==as.numeric(region_table$chr) & ((region_start()<=region_table$region_start_250kb & region_end()>=region_table$region_start_250kb) | (region_start()>=region_table$region_start_250kb & region_start()<=region_table$region_end_250kb))),]
		}else{0}
	  })
	  
	  overlap_loci <- eventReactive(go$region,{nrow(ov_loci())!=0}) # TRUE or FALSE if there are overlapping loci
	  
	  output$ov_loci <- DT::renderDT(server=FALSE, { # rendering the region table of the overlapping loci
		if(valid_reg() & overlap_loci()){
		  datatable(ov_loci(),rownames=FALSE,
					options = list(
					  columnDefs = list(list(className = 'dt-left', targets = "_all"))
					  , scrollX=TRUE, scrollCollapse = TRUE,
					  dom = 'lfrtBip',
					  buttons= list(c('copy', 'excel','csv'))
					),
					extensions = 'Buttons',
					caption = htmltools::tags$caption(
					  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
					  '* SNPs, which are associated with log(eGFRcrea) in the all-ancestry GWAS meta-analysis (unconditioned) with P<5E-8')
					
					)
		}
	  })
	  
	  output$ov_loci_test <- renderText({
		
		if(valid_reg()& !overlap_loci()) {
		  "no eGFRcrea associated loci found in this region"
		}else{
		  if(valid_reg() & overlap_loci()){
			paste(length(unique(ov_loci()[,1]))," loci (including ",nrow(ov_loci())," signals) have been found in your searched region.", sep="")
		  }
		}
	  })
	  
	  GPS_columns_region <- match(c('Gene*','Locus name**','Locus ID','Signal ID','eGFRcys or BUN validation','DM_effect','decline_effect','# credible variants in signal','max PPA','stop-gained, stop-lost, non-synonymus','canonical splice, noncoding change, synonymous, splice site','other deleterious variant','eQTL glomerulus (NEPTUNE, or Sheng et al [Nat Genet, 2021])','eQTL tubulo-interstitium (NEPTUNE, or Sheng et al [Nat Genet, 2021])','eQTL kidney cortex (GTEx)','eQTL other tissue (GTEx)','sQTL kidney cortex (GTEx)','sQTL other tissue (GTEx)','Coloc in NEPTUNE tissue','# kidney phenotypes in mouse','# kidney phenotypes in human','known_drug_target','Distance to locus lead variant','Chromosome','Position gene start','Position gene end'),names(GPS_show))
	  GPS_region_search <- reactive({
		if(valid_reg()&overlap_loci()){
		  x = GPS_show[which(GPS$locus_id%in%ov_loci()[,1]),GPS_columns_region]
		  y = GPS[which(GPS$locus_id%in%ov_loci()[,1]),]
		  v=c()
		  for(i in 1:nrow(y)){
		  v[i] = 11-length(which(y[i,c(9:19)]==0)) # check how many non zero annotation columns per gene there are
		  }
		  # x['Genescore']=v
		  # x=x[,c(1:6,23,7:22)]
		  x[order(v,decreasing=T),] # order GPS Table that Genes with most entries are at the top
		  
		}else{NULL}
	  })
	  
	  output$GPS_region_search <- 
		DT::renderDT(server=FALSE, { #render GPS Table
		  if(valid_reg()& !is.null(GPS_region_search())){
			datatable(GPS_region_search(),rownames=FALSE,
					  
					  
					  options = list(
						columnDefs = list(list(className = 'dt-left', targets = "_all"))
						, scrollX=TRUE, scrollCollapse = TRUE
						,headerCallback = JS(headerCallback2),
						dom = 'lfrtBip',
						buttons= list(c('copy', 'excel','csv'))
						
					  ),
					  extensions= 'Buttons',
					  class ='display',
					  filter = list(
						position = 'top', clear = TRUE, plain = TRUE
					  ),
					  escape = FALSE,
					  
					  caption = htmltools::tags$caption(
						style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						'* Link to GeneCards, **nearest gene to locus lead-variant'),
					  container = sketch2
			)%>% formatPercentage('max PPA') #format number to percentage
		  } 
		})
	  
	  observeEvent(go$region,{shinyjs::showElement(id="region_div")}) # show results section when search is started
	  observeEvent(go$region, ({
		
		if(valid_reg()){
		  updateCollapse(session, "regionCollapse", open = c("Overlapping genetic eGFRcrea signals")) # open region search section
		  if(!is.null(GPS_region_search())){
			  updateCollapse(session, "regionCollapse", open = c("Overlapping genetic eGFRcrea signals", "GPS entries for genes in overlapping eGFRcrea loci")) # open GPS section, when there is a overlapping locus
			
		  }
		}
	  }))
	  
	  
	  observe({
		
		shinyjs::toggleElement(id="ov_genes_down", condition = (!is.null(GPS_region_search())) ) #make GPS Table visible
		
	  })
	  
	  
	  #help window handeling:
	  observeEvent(input$question_region_search,{ 
		showModal(modalDialog(
		  titel="Help region search",
		  tags$h4("Region search - Help:"),
		  p("Select a chromosome and enter a start/end position [bp] of a region you are interested in. The results show you a list of overlapping eGFRcrea loci and the GPS entries for the respective genes if there are any. Attation: The region end must be larger than region start and region start can't be 0. For further information check the Documentation page."),
		  easyClose = TRUE,
		  footer=NULL,
		  size = "m"
		))
	  })

	# snp page ----------------------------------------------------------------
	  
	  #define input type based on click for further analyses
	  inputtype <- reactiveValues('snp'=c())
	  onclick("snp", (inputtype$snp=c("single")), add=T)
	  onclick("SNP_batch", (inputtype$snp=c("list")), add=T)
	  onclick("snp-upload-section", (inputtype$snp=c("upload")), add=T)
	  
	  #show search button:
	  
	  observe(
		if(isTruthy(input$file1)|isTruthy(input$snp)|isTruthy(input$SNP_batch)){
		  shinyjs::showElement(id="snp_go_div")
		}
	  )
	  
	  
	  #combine ENTER and "go" click to one reactive value only reacting if there is snp input
	  go <- reactiveValues('snp'=c())
	  
	  #update the value of snp$go when go or enter are clicked:
	  observeEvent(input$snp_go, {
		if(isTruthy(input$file1)|isTruthy(input$snp)|isTruthy(input$SNP_batch)){
		  if(is.null(go$snp)){go$snp=1}
		  else{go$snp <- go$snp+1}
		  
		}
	  })
	  observeEvent(input$keyPressed, {
		if(isTruthy(input$file1)|isTruthy(input$snp)|isTruthy(input$SNP_batch)){
		  if(is.null(go$snp)){go$snp=1}
		  else{go$snp <- go$snp+3}
		}
	  })
	  
	  #upload options visible when there is an input file
	  observe({
		shinyjs::toggleElement(id="SNP_batch_upload_options", condition = (isTruthy(input$file1)) )
	  })
	  
	  #disable credvar only functions, when not searched for cred vars
	  observe({
		
		if(any(input$`variant-search-options`=="credvar")){
		output$detail.evidence.snp <- renderUI(
		div(class="snpinputbox",checkboxGroupInput(inputId="detail_evidence_variant", label=span(class="snpsearch","Specification of additional functional SNP information",span(class="snpsearchcomment","functional information only available for credible variants")),  selected = c("CADD","NEPTUNE_glo","NEPTUNE_tub","GTEx_eQTL","GTEx_sQTL"),
												   inline = FALSE, width = NULL, choiceNames = c("deleteriousness and functional annotation (CADD)","eQTL in glomerulus (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in tubulo-interstitium (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in kidney cortex (GTEx)","sQTL in kidney cortex (GTEx)","eQTL in other tissues (GTEx)","sQTL in other tissues (GTEx)", "eGFR association statistics separated by diabetes status"), choiceValues = c("CADD","NEPTUNE_glo","NEPTUNE_tub","GTEx_eQTL","GTEx_sQTL","GTEx_wo_kidney_eQTL","GTEx_wo_kidney_sQTL","dm_stat")),
		))}else{
		  output$detail.evidence.snp <- renderUI(
			div(class="snpinputbox",disabled(checkboxGroupInput(inputId="detail_evidence_variant", label=span(class="snpsearch","Specification of additional functional SNP information or locus based information",span(class="snpsearchcomment","functional information only available for credible variants")),
													   inline = FALSE, width = NULL, choiceNames = c("deleteriousness and functional annotation (CADD)","eQTL in glomerulus (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in tubulo-interstitium (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in kidney cortex (GTEx)","sQTL in kidney cortex (GTEx)","eQTL in other tissues (GTEx)","sQTL in other tissues (GTEx)","eGFR association statistics separated by diabetes status"), choiceValues = c("CADD","NEPTUNE_glo","NEPTUNE_tub","GTEx_eQTL","GTEx_sQTL","GTEx_wo_kidney_eQTL","GTEx_wo_kidney_sQTL","dm_stat"))),
			))
		}
	  })
	  
	  ###first snp input processing: 
	  
	  single_snp <- eventReactive(go$snp, {
		gsub(" ","",input$snp) # exclude spaces
		})
	  
	  #build vector of snps from text input
	  input_snp_batch_list <- eventReactive(go$snp,{
		
		req(input$SNP_batch) #only do the following when there is input
		
		ivbl=unlist(strsplit(input$SNP_batch,"[^[:alnum:]^:]",perl=TRUE)) ## excludes all non-word characteres except ":" that might be used as separators
		if(any(ivbl=="")){
		  ivbl[-which(ivbl=="")]
		}else{ivbl}
	  })
	  
	  
	  #build vector of snps from file input
	  
	  input_snp_batch_upload <- reactiveValues('input'= c())
	  
	  observe({
		req(input$file1)
		
		# df <- read.table(input$file1$datapath,
		#                  sep = input$sep_snp,
		#                  quote = input$quote_snp)
		# 
		# input_snp_batch_upload$input = paste(df[1,])
		input_snp_batch_upload$input <- scan(file=input$file1$datapath,
											 sep = input$sep_snp,
											 what="character")
		
	  })
	  
	  
	  #reset file input, when reset button is clicked
	  observeEvent(input$reset_variant,{
		reset("file1")
		input_snp_batch_upload$input <- NULL
	  })
	  
	  #final selection and save searched snps in "snp()":
	  snp <- eventReactive(go$snp, {
		
		if(inputtype$snp=="single"){
		  single_snp()
		}else{
		  if(inputtype$snp=="list"){
			input_snp_batch_list()
		  }else{
			if(inputtype$snp=="upload"){
			  input_snp_batch_upload$input
			}else{NULL}
		  }
		}
		})
	  
	  inputtype <- reactiveValues('snp_final'=c()) #use inputtype$snp_final as it only reacts on "search"-click
	  observeEvent(go$snp,{
		inputtype$snp_final = inputtype$snp 
	  })
	  
	  #snp page	output generation:
	  
	  #search for overlap with credible variants
	  cred_vars <- eventReactive(go$snp, {
		if(!is.null(snp())){
			snp1 = snp()
			if(any(grepl(":", snp1))){ # do this when chr:pos is used as input
				for(i in grep(":", snp1)){
					chr=unlist(strsplit(snp1[i],":"))[1]
					pos=unlist(strsplit(snp1[i],":"))[2]
					if(any(cred_var$chr==chr & cred_var$pos==pos)){
						snp1[i] = cred_var$rsid[which(cred_var$chr==chr & cred_var$pos==pos)] # replace searched input (chr:pos) with rsids
					}
				}
			}
		  if(any(snp1%in%cred_var$rsid)){
			snp_in_cred=(snp1%in%cred_var$rsid)
			snp1[which(snp_in_cred)] # restrict to SNPs included in 99% credible set variants
		  }else{NULL}
		  }else{NULL}
		})
	  
	  valid <- eventReactive(go$snp, {!is.null(cred_vars())}) #any of the searched variants is a credible variant --> TRUE
	  
	  detail_evidence_variant <- eventReactive(go$snp, {input$detail_evidence_variant})
	  
	  gws_snps <- eventReactive(go$snp,{
		if(!is.null(snp())){
		  if(any(snp()%in%all_sig$RSID)){
		  snp_in_gws = (snp()%in%all_sig$RSID)
		  snp()[which(snp_in_gws)] # search for overlap with genome-wide sigificant variants
		  }else{NULL}
		  }else{NULL}
	  })
	  
	  gws<-eventReactive(go$snp, {!is.null(gws_snps())}) # any of the searched variants is gws
	  
	  # open all relevant divs:
	  observeEvent(go$snp,{shinyjs::showElement(id="SNP_div")})
	  observeEvent(go$snp,{runjs('
		  document.getElementById("SNP_div").scrollIntoView({ left: 0, block: "end", behavior: "smooth" }); 
		')}) # the previous java-script command leads to automatic scrolling
	  observeEvent(go$snp,{shinyjs::toggleElement(id="gws_div", condition = any(input$`variant-search-options`=="gws"))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="cred_var_div", condition = any(input$`variant-search-options`=="credvar"))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_cadd_snp", condition= (any(detail_evidence_variant()=="CADD")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_glo_snp", condition= (any(detail_evidence_variant()=="NEPTUNE_glo")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_tub_snp", condition= (any(detail_evidence_variant()=="NEPTUNE_tub")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_GTEx_eqtl_snp", condition= (any(detail_evidence_variant()=="GTEx_eQTL")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_GTEx_sqtl_snp", condition= (any(detail_evidence_variant()=="GTEx_sQTL")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_GTEx_eqtl_wo_kidney_snp", condition= (any(detail_evidence_variant()=="GTEx_wo_kidney_eQTL")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_GTEx_sqtl_wo_kidney_snp", condition= (any(detail_evidence_variant()=="GTEx_wo_kidney_sQTL")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_dm_stat_snp", condition= (any(detail_evidence_variant()=="dm_stat")&valid()))})
	  observeEvent(go$snp,{shinyjs::toggleElement(id="div_locus_zoom_snp", condition= (any(input$snp.locuszoom=="locus_zoom")&valid()))})
	  
	  
	  #generate text output how many searched snps are genome-wide significant associated
	  output$namecheck <- renderText({
		if(!is.null(snp())){
		  if(inputtype$snp_final=="single"){
			if(gws()){
			  paste(snp(), "is associated with log(eGFRcrea) in at least one of the 424 loci at P<5E-8 (all-ancestry GWAS meta-analysis, unconditioned):", sep=" ")
			}else{
			  paste(snp(), "is not included in the analysis, or not associated with log(eGFRcrea) in one of the 424 loci at P<5E-8 (all-ancestry GWAS meta-analysis, unconditioned).", sep=" ")
			}
		  }else{
			  if(gws()){
				if(length(gws_snps())>1){
				  paste("Of your ",length(snp()), " queried SNPs, ", length(gws_snps()), " SNPs are associated with log(eGFRcrea) in at least one of the 424 loci at P<5E-8 (all-ancestry GWAS meta-analysis, unconditioned):", sep="")
				}else{
				  paste("Of your ",length(snp()), " queried SNPs, ", length(gws_snps()), " SNP (", gws_snps(),") is associated with log(eGFRcrea) in at least one of the 424 loci at P<5E-8 (all-ancestry GWAS meta-analysis, unconditioned):", sep="") 
				  }
			  }else{
				paste("None of your ",length(snp()), " queried SNPs is associated with log(eGFRcrea) in one of the 424 loci at P<5E-8 (all-ancestry GWAS meta-analysis, unconditioned).", sep="") 
			  }
			}
		  
		}else{
			"Please check if you correctly entered or uploaded your SNPs and if the right input field is selected."
		  }
	  })
	  
	  #render table of all-ancestry statistics
	  output$all_var <- DT::renderDT(server=FALSE, { 
		if(gws()){
		  datatable(all_sig_show[which(all_sig$RSID%in%gws_snps()),c(15,11,12,2:8,22,23,24)], rownames=FALSE,
					options = list(
					  columnDefs = list(list(className = 'dt-left', targets = "_all"))
					  , scrollX=TRUE, scrollCollapse = TRUE,
					  dom = 'lfrtBip',
					  buttons= list(c('copy', 'excel','csv'))
					),
					extensions = 'Buttons',
					escape=FALSE,
					caption = htmltools::tags$caption(
					  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
					  htmltools::tags$p('* Link to dbSNP'),
					  htmltools::tags$p('** adjusted for age, sex and other study-specific covariates'),
					  htmltools::tags$p('Effect allele: eGFRcrea lowering allele, N: Sample size of the all-ancestry GWAS meta-analysis for this SNP, EAF: Effect allele frequency'),
					  htmltools::tags$p('beta: Effectsize of the effect allele on log(eGFRcrea)')
					  ),
					class='display',
					container = sketchallsig
					)
		}
	  })
	  
	  #generate text output of how many searched SNPs are credible set variants
	  output$namecheck2 <- renderText({
		if(!is.null(snp())){
		  if(inputtype$snp_final=="single"){
			if(valid()){
			  paste(snp(), "is a 99% credible variant in at least one of the 594 eGFRcrea signals (EUR). Shown are results from GWAS meta-analyses in European ancestry:", sep=" ")
			}else{
			  paste(snp(), "is not a 99% credible variant in any of the 594 eGFRcrea signals (EUR).", sep=" ")
			}
		  }else{
			if(gws()){
			  if(length(cred_vars())>1){
				paste("Of your ",length(snp()), " queried SNPs, ", length(cred_vars()), " SNPs are 99% credible variants in at least one of the 594 eGFRcrea signals (EUR). Shown are results from GWAS meta-analyses in European ancestry:", sep="")
			  }else{
				paste("Of your ",length(snp()), " queried SNPs, ", length(cred_vars()), " SNP (", cred_vars(),") is a 99% credible variant in at least one of the 594 eGFRcrea signals (EUR). Shown are results from GWAS meta-analyses in European ancestry:", sep="") 
			  }
			}else{
			  paste("None of your ",length(snp()), " queried SNPs is a 99% credible variant in any of the 594 eGFRcrea signals (EUR).", sep="") 
			}
		  }
		  
		}
	  })
	  
	  # render credible set table (EUR association statistics conditioned and unconditioned)
	  output$cred_var <- DT::renderDT(server=FALSE, {
		if(valid()){
		  datatable(cred_var_show[which(cred_var$rsid%in%cred_vars()),], rownames=FALSE,
					options = list(
					  columnDefs = list(list(className = 'dt-left', targets = "_all"))
					  , scrollX=TRUE, scrollCollapse = TRUE,
					  dom = 'lfrtBip',
					  buttons= list(c('copy', 'excel','csv'))
					),
					extensions = 'Buttons',
					escape=FALSE,
					caption = htmltools::tags$caption(
					  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
					  htmltools::tags$p('* Link to dbSNP'),
					  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal)'),
					  htmltools::tags$p('N: Sample size of the European-ancestry GWAS meta-analysis for this SNP, Effect allele: eGFRcrea lowering allele, EAF: Effect allele frequency'),
					  htmltools::tags$p('Unconditioned association statistics origin from a european ancestry only GWAS meta-analysis for eGFRcrea. For loci with multiple independent signals, the conditioned association statistics origin from conditioning on the signal index variants of the other signals in the locus.')
					  ),
					class='display',
					container = sketchcredvar
					)%>% formatPercentage('PPA')
		}
	  })
	  
	  # Text if variants have functional consequences
	  output$extras <-  renderText({
		if(valid()){
		  if(length(detail_evidence_variant())!=0){
			
			  paste("The following functional evidence exists for the ",length(cred_vars())," 99% credible variant(s) included in your search:",sep="")
			
		  }
		}else{
		  "Your request didn't include any 99% credible variant so no search for functional evidence can be performed."
		}
	  })
	  
	  
	  #output CADD
	  output$CADD_text <- renderText({
		
		  if(any(detail_evidence_variant()=="CADD")){
			if(any(CADD$RSID%in%cred_vars())){
			  if(length(cred_vars())==1){
				paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%CADD$RSID))," SNP resides in a gene and is predicted deleterious:",sep="")
			  }else{
				paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%CADD$RSID))," SNP(s) reside(s) in a gene and is/are predicted deleterious:",sep="")  
			  }
			}else{
			  if(length(cred_vars())==1){
				paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not reside in a gene or is not predicted deleterious.",sep="")
			  }else{
				paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, resides in a gene and is predicted deleterious.",sep="")  
			  }
			}
		  }
	  })
	  
	  output$CADD <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="CADD")){
			if(any(CADD$RSID%in%cred_vars())){
			  x = CADD_show[which(CADD$RSID%in%cred_vars()),]
			  if(any(duplicated(x))) x = x[-which(duplicated(x)),]
			  datatable(x, rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal)'),
						  htmltools::tags$p(' Reference and alternative allele from human reference genome (GRCh37)')
						),
						class ='display',
						container = sketchcadd
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  # Neptune + Susztak glomerulus
	  output$glo_text <- renderText({
		
		  if(any(detail_evidence_variant()=="NEPTUNE_glo")){
			if(any(glo_show$RSID%in%cred_vars())){
			  if(length(cred_vars())==1){
				paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%glo_show$RSID))," SNP modulates gene expression in glomerular tissue:",sep="")
			  }else{
				paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%glo_show$RSID))," SNP(s) modulate(s) gene expression in glomerular tissue:",sep="")  
			  }
			}else{
			  if(length(cred_vars())==1){
				paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene expression in glomerular tissue.",sep="")
			  }else{
				paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene expression in glomerular tissue.",sep="")  
			  }
			}
		  }
		
	  })
	  
	  output$glo <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="NEPTUNE_glo")){
			if(any(glo_show$RSID%in%cred_vars())){
			  datatable(glo_show[which(glo_show$RSID%in%cred_vars()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						  htmltools::tags$p('** NEPTUNE, or Sheng et al, Nat.Genet. 2021')
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #Neptune tubulo-interstitium
	  output$tub_text <- renderText({
		
		  if(any(detail_evidence_variant()=="NEPTUNE_tub")){
			if(any(tub_show$RSID%in%cred_vars())){
			  if(length(cred_vars())==1){
				paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%tub_show$RSID))," SNP modulates gene expression in tubulo-interstitial tissue:",sep="")
			  }else{
				paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%tub_show$RSID))," SNP(s) modulate(s) gene expression in tubulo-interstitial tissue:",sep="")  
			  }
			}else{
			  if(length(cred_vars())==1){
				paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene expression in tubulo-interstitial tissue.",sep="")
			  }else{
				paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene expression in tubulo-interstitial tissue.",sep="")  
			  }
			}
		  }
		
	  })
	  
	  output$tub <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="NEPTUNE_tub")){
			if(any(tub_show$RSID%in%cred_vars())){
			  datatable(tub_show[which(tub_show$RSID%in%cred_vars()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						  htmltools::tags$p('** NEPTUNE, or Sheng et al, Nat.Genet. 2021')
						  )
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #GTEx eQTL kidney
	  output$eqtl_text <- renderText({
		
		  if(any(detail_evidence_variant()=="GTEx_eQTL")){
			if(any(GTEx_eQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  if(length(cred_vars())==1){
				paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%GTEx_eQTL$rs_id_dbSNP151_GRCh38p7))," SNP modulates gene expression in kidney cortex tissue:",sep="")
			  }else{
				paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%GTEx_eQTL$rs_id_dbSNP151_GRCh38p7))," SNP(s) modulate(s) gene expression in kidney cortex tissue:",sep="")  
			  }
			}else{
			  if(length(cred_vars())==1){
				paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene expression in kidney cortex tissue.",sep="")
			  }else{
				paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene expression in kidney cortex tissue.",sep="")  
			  }
			}
		  }
		
	  })
	  
	  output$eqtl <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="GTEx_eQTL")){
			if(any(GTEx_eQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  datatable(GTEx_eQTL_show[which(GTEx_eQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #GTEx sQTL kidney
	  output$sqtl_text <- renderText({
		
		  if(any(detail_evidence_variant()=="GTEx_sQTL")){
			if(any(GTEx_sQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  if(length(cred_vars())==1){
				paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%GTEx_sQTL$rs_id_dbSNP151_GRCh38p7))," SNP modulates gene splicing in kidney cortex tissue:",sep="")
			  }else{
				paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%GTEx_sQTL$rs_id_dbSNP151_GRCh38p7))," SNP(s) modulate(s) gene splicing in kidney cortex tissue:",sep="")  
			  }
			}else{
			  if(length(cred_vars())==1){
				paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene splicing in kidney cortex tissue.",sep="")
			  }else{
				paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene splicing in kidney cortex tissue.",sep="")  
			  }
			 }
		  }
		
	  })
	  
	  output$sqtl <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="GTEx_sQTL")){
			if(any(GTEx_sQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  datatable(GTEx_sQTL_show[which(GTEx_sQTL$rs_id_dbSNP151_GRCh38p7%in%cred_vars()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* sQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #GTEx eQTL other tissue
	  output$eqtl_wo_kidney_text <- renderText({
		
		if(any(detail_evidence_variant()=="GTEx_wo_kidney_eQTL")){
		  if(any(gtex_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			if(length(cred_vars())==1){
			  paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%gtex_without_kidney$rs_id_dbSNP151_GRCh38p7))," SNP modulates gene expression in other tissues:",sep="")
			}else{
			  paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%gtex_without_kidney$rs_id_dbSNP151_GRCh38p7))," SNP(s) modulate(s) gene expression in other tissues:",sep="")  
			}
		  }else{
			if(length(cred_vars())==1){
			  paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene expression in other tissues.",sep="")
			}else{
			  paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene expression in other tissues.",sep="")  
			}
		  }
		}
		
	  })
	  
	  output$eqtl_wo_kidney <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="GTEx_wo_kidney_eQTL")){
			if(any(gtex_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  datatable(gtex_without_kidney_show[which(gtex_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars()),c(1:13)], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
			  )%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #GTEx sQTL other tissue
	  output$sqtl_wo_kidney_text <- renderText({
		
		if(any(detail_evidence_variant()=="GTEx_wo_kidney_sQTL")){
		  if(any(sqtl_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			if(length(cred_vars())==1){
			  paste("Of your ",length(cred_vars())," searched SNP that is a credible variant, ",length(which(cred_vars()%in%sqtl_without_kidney$rs_id_dbSNP151_GRCh38p7))," SNP modulates gene splicing in other tissues:",sep="")
			}else{
			  paste("Of your",length(cred_vars()) ," searched SNPs that are credible variants, ",length(which(cred_vars()%in%sqtl_without_kidney$rs_id_dbSNP151_GRCh38p7))," SNP(s) modulate(s) gene splicing in other tissues:",sep="")  
			}
		  }else{
			if(length(cred_vars())==1){
			  paste("Your ",length(cred_vars())," searched SNP, that is a credible variant, does not modulate gene splicing in other tissues.",sep="")
			}else{
			  paste("None of your ",length(cred_vars())," searched SNPs, that are credible variants, modulates gene splicing in other tissues.",sep="")  
			}
		  }
		}
		
	  })
	  
	  output$sqtl_wo_kidney <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="GTEx_wo_kidney_sQTL")){
			if(any(sqtl_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars())){
			  datatable(sqtl_without_kidney_show[which(sqtl_without_kidney$rs_id_dbSNP151_GRCh38p7%in%cred_vars()),c(1:12)], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* sQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
			  )%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  # statistics separated by DM status
	  output$dm_stat_snp <- DT::renderDT(server=FALSE, {
		if(valid()){
		  if(any(detail_evidence_variant()=="dm_stat")){
			if(any(DM_stats$rsid%in%cred_vars())){
			  datatable(DM_stats_show[which(DM_stats$rsid%in%cred_vars()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('N: Sample size of European-ancestry GWAS meta-analysis for this SNP, Effect allele: eGFRcrea lowering allele in the combined analysis, EAF: Effect allele frequency')
						),
						class='display',
						container = sketchdmstatgene
			  )
			}
		  }
		}
	  })
	  
	  # help window SNP-search:
	  observeEvent(input$question_SNP,{
		showModal(modalDialog(
		  titel="Help SNP search",
		  tags$h4("Variant search - Help:"),
		  p("Enter a RSid or genomic position in format of chromosome:position (GRCh37) of a SNP you are interested in. If you search without any additional options the association statistics for unconditioned eGFRcrea (if the SNP has an association p-value <5E-8) are displayed. If the SNP is a 99% credible variant for any eGFRcrea signal also conditioned results (if there are other independent signals in the respective locus) and the posterior propabitliy of association (PPA) is shown in a second table. For further information check the Documentation & Help page."),
		  easyClose = TRUE,
		  footer=NULL,
		  size = "m"
		))
	  })
	  
	  #Locus Zoom
	  Locus_ID_snp<-reactive({
		if(valid())
		{cred_var$`Locus Id`[which(cred_var$rsid==snp())]}
	  })
	  
	  #create HTML syntax for link to plots
	  snp_plots <- reactive({
		if(valid()){
		  ids=which(links_id%in%Locus_ID_snp())[1] ## preliminary only one plot per locus, should be overall plot
		  paste("<p><a href=\"",links[ids],"\"target=\"_blank\">Regional association plot of locus",links_id[ids],"</a></p>")
		}
	  })
	  
	  #render links
	  output$plot_download_links_snp <- renderUI({
		list(lapply(snp_plots(),HTML))
	  })
	  
	  
	  # filename1_snp <- reactive({
	  #   if(valid()&inputtype$snp_final=="single"){
	  #     if(any(input$snp.locuszoom=="locus_zoom")){
	  #       as.character(lz_files[which(locus_id_files==as.character(Locus_ID_snp()))][1])
	  #       
	  #     }
	  #   }
	  # })
	  # 
	  # 
	  # output$LocusZoom_SNP <- renderUI({
	  #   req(filename1_snp())
	  #   
	  #   tags$img(height="100%",
	  #            width="100%",
	  #            src=filename1_snp())
	  # })
	  
	  
	  

	# gene page ---------------------------------------------------------------
	  
	  
	  #define input type based on click for further analyses
	  inputtype <- reactiveValues('gene'=c())
	  onclick("Genename", (inputtype$gene=c("single")), add=T)
	  onclick("gene_batch", (inputtype$gene=c("list")), add=T)
	  onclick("gene-upload-section", (inputtype$gene=c("upload")), add=T)
	  
	  
	  observe({
		
		shinyjs::toggleElement(id="gene_batch_upload_options", condition = (isTruthy(input$file2)) )
		
	  })
	  
	  #show search button:
	  
	  observe(
		if(isTruthy(input$file2)|isTruthy(input$Genename)|isTruthy(input$gene_batch)){
		  shinyjs::showElement(id="gene_go_div")
		}
	  )
	  
	  
	  #combine ENTER and "go" click to one reactive value only reacting if there is gene input and change that value each time "go" or ENTER are clicked
	  go <- reactiveValues('gene'=c(c()))
	  
	  observeEvent(input$gene_go, {
		if(isTruthy(input$file2)|isTruthy(input$Genename)|isTruthy(input$gene_batch)){
		  if(is.null(go$gene)){go$gene=1}
		  else{go$gene <- go$gene+1}
			
		}
	  })
	  observeEvent(input$keyPressed, {
		if(isTruthy(input$file2)|isTruthy(input$Genename)|isTruthy(input$gene_batch)){
		  if(is.null(go$gene)){go$gene=1}
		  else{go$gene <- go$gene+3}
		}
		})
	  
	   
	  #output$testforme <- renderText({as.integer(go$gene)}) <- test option
	  
	  ###first gene input processing: 
	  single_gene <- eventReactive(go$gene, {input$Genename})
	  
	  ###build vector of genes from text input
	  input_gene_batch_list <- eventReactive(go$gene,{
		
		req(input$gene_batch)
		
		igbl=unlist(strsplit(input$gene_batch,"[^[:alnum:]^-]",perl=TRUE)) ## excludes all non-word characteres except "-" that might be used as separators
		if(any(igbl=="")){
		  igbl[-which(igbl=="")]
		}else{igbl}
	  })
	  
	  
	  ###build vector of genes from file input
	  
	  input_gene_batch_upload <- reactiveValues('input'= c())
		
	  observe({
		req(input$file2)
		
		# df <- read.table(input$file2$datapath,
		#                  sep = input$sep_gene,
		#                  quote = input$quote_gene)
		# 
		# input_gene_batch_upload$input = paste(df[1,])
		input_gene_batch_upload$input <- scan(file=input$file2$datapath,
											  sep = input$sep_gene,
											  what="character")
		
	  }) 
	  
	  #reset file input, when reset button is clicked
	  observeEvent(input$reset_gene,{
		reset("file2")
		input_gene_batch_upload$input <- NULL
	  })
	  
	  #final selection:
	  gene <- eventReactive(go$gene, {
		
		if(inputtype$gene=="single"){
		  toupper(single_gene())
		}else{
		  if(inputtype$gene=="list"){
			toupper(input_gene_batch_list())
		  }else{
			if(inputtype$gene=="upload"){
			  toupper(input_gene_batch_upload$input)
			}else{NULL}
		  }
		}
	  })
	  
	  inputtype <- reactiveValues('gene_final'=c()) #use inputtype$gene_final as it only reacts on "search"-click
	  observeEvent(go$gene,{
		inputtype$gene_final = inputtype$gene 
	  })
	  
	  
	  
	  
	  # skript for gene page
	  corrected_genenames <- eventReactive(go$gene,{ # indices vector if input genenames are not HGNC names 
		if(!is.null(gene())){
		  if(any(gene()%in%toupper(real_genes$genes) & !gene()%in%toupper(real_genes$Approved.symbol) )){
			which(gene()%in%toupper(real_genes$genes) & !gene()%in%toupper(real_genes$Approved.symbol)) # should provide indices, when gene is not HGNC gene
		  }else{NULL}
		}else{NULL}
	  })
	  
	  
	  correct_genenames <- eventReactive(go$gene,{
		if(!is.null(gene())){
			gene1 = gene()
			if(!is.null(corrected_genenames())){ # if there are genes which are synonyms do the following
				
				for (i in corrected_genenames()){
					gene1[i]=toupper(real_genes$Approved.symbol[which(toupper(real_genes$genes)==gene1[i])]) # correct the input gene names to HGNC gene names
				}
			}
			if(any(gene1%in%toupper(genes_sorted$Gene))){
				gene1[which(gene1%in%toupper(genes_sorted$Gene))] #check for overlap with gene-list that was used to generate KidneyGPS
			}else{NULL}
		}else{NULL}
	  })
	  
	  #Render Text for Synoym-check:
	  output$testforme <-  renderText({
		if(!is.null(gene())){
			if(!is.null(corrected_genenames())){
				if(length(corrected_genenames())==1){
					paste("Of your ",length(gene()), " gene names(s), ", length(corrected_genenames()), " was a synonym and was matched to its  official HGNC gene name [",correct_genenames(),"].")
				}else{
					paste("Of your ",length(gene()), " gene names(s), ", length(corrected_genenames()), " were a synonyms and were matched to their  official HGNC gene names.")
				}
			}
		}
	  })
	  
	  #check for overlap of correct gene-names with GPS-Genes:
	  genes_in_gps <- eventReactive(go$gene,{
		if(!is.null(correct_genenames())){
		  if(any(correct_genenames()%in%toupper(GPS$Gene))){
			correct_genenames()[which(correct_genenames()%in%toupper(GPS$Gene))]
		  }else{NULL}
		}else{NULL}
	  })
	  
	  valid_gene <- eventReactive(go$gene, {!is.null(genes_in_gps())}) # TRUE when there is overlap of searched genes with GPS-genes
	  
	  # combine mapping input options to one value. Contains the indices of GPS columns referring to the selected options:
	  columns <- eventReactive(go$gene, {if(isTruthy(input$columns1)|isTruthy(input$columns2)|isTruthy(input$columns3)|isTruthy(input$columns4)){ as.numeric(c(input$columns1,input$columns2,input$columns3,input$columns4))}else{NULL}})
	  #detail_evidence_gene <- eventReactive(go$gene, {c(input$detail_evidence_gene_1,input$detail_evidence_gene_2,input$detail_evidence_gene_3)})
	  
	  detail_evidence_locus_gene <- eventReactive(go$gene, {input$detail_evidence_locus_gene}) # save additional options in this value, that only changes when go$gene changes
	  
	  ppa <- eventReactive(go$gene, {input$ppa}) # save ppa input in this value when go$gene changes
	  
	  incorrect_genenames <- eventReactive(go$gene,{ # calculate number of incorrect gene-names
		length(gene())-length(correct_genenames())
	  })
	  
	  # render text for gene search depending of how many input genes are real gene-names and how many overlap KidneyGPS genes:
	  output$namecheck_gene <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.null(correct_genenames())){
			paste(gene()," is no valid/official gene name, please check your input.",sep="")
		  }else{
			if(is.null(genes_in_gps())){
			  paste(correct_genenames()," is not included in any eGFRcrea locus.", sep="")
			}else{
				if(!is.null(corrected_genenames())){
					paste("Following information has been found for ", genes_in_gps(),"[",gene(),"]",":",sep="")
				}else{
					paste("Following information has been found for ", genes_in_gps(),":",sep="")
				}
			}
		  }
		}else{
		  if(is.null(correct_genenames)){
			if(incorrect_genenames()==1){
			  paste("You queried ",length(gene())," gene name. ",gene(),"  is no official/valid gene name. No further searches were performed. Please check your input.",sep="")
			}else{
			  paste("You queried ",length(gene())," gene names. All of these are no official/valid gene names. No further searches were performed. Please check your input.",sep="")
			}
		  }else{
			if(is.null(genes_in_gps())){
			  if(incorrect_genenames()==1){
				paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these is no official/valid gene name. Additionally, none of your ", length(correct_genenames()), " queried official genes is not included in any of the 424 eGFRcrea loci. Thus, no GPS entries are available.",sep="")
			  }else{
				if(incorrect_genenames()==0){
				  paste("You queried ",length(gene())," genes. All of these are official/valid gene names. However, none of your ", length(correct_genenames()), " queried official genes is included in any of the 424 eGFRcrea loci. Thus, no GPS entries are available.",sep="")
				}else{
				  paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these are no official/valid gene names. Additionally, none of your ", length(correct_genenames()), " queried official genes is included in any of the 424 eGFRcrea loci. Thus, no GPS entries are available.",sep="")
				}
			  }
			}else{
			  if(length(correct_genenames())==1){
				if(incorrect_genenames()==1){
				  paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these is no official/valid gene names. ",length(genes_in_gps()), " (",genes_in_gps(),")"," of your ", length(correct_genenames()), " queried official genes is included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="")
				}else{
				  if(incorrect_genenames()==0){
					paste("You queried ",length(gene())," genes. All of these are official/valid gene names from HGNC. ",length(genes_in_gps())," (",genes_in_gps(),")"," of your ", length(correct_genenames()), " queried official genes is included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="")
				  }else{
					paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these are no official/valid gene names. ",length(genes_in_gps())," (",genes_in_gps(),")"," of your ", length(correct_genenames()), " queried official genes is included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="")
				  }
				}
			  }else{
				if(incorrect_genenames()==1){
				  paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these is no official/valid gene names. ",length(genes_in_gps())," of your ", length(correct_genenames()), " queried official genes are included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="") 
				}else{
				  if(incorrect_genenames()==0){
					paste("You queried ",length(gene())," genes. All of these are official/valid gene names from HGNC. ",length(genes_in_gps())," of your ", length(correct_genenames()), " queried official genes are included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="") 
				  }else{
					paste("You queried ",length(gene())," genes. ",incorrect_genenames()," of these are no official/valid gene names. ",length(genes_in_gps())," of your ", length(correct_genenames()), " queried official genes are included in any of the 424 eGFRcrea loci and thus included in the GPS.",sep="") 
				  }
				}
			  }
			}
		  }
		}
	  })
	  
	  
	 
	  #CADD: 
	  
	  # render CADD output-table
	  output$CADD_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()%in%c(11:13))){
			if(any(toupper(CADD$GeneName)%in%genes_in_gps())){
			  x = CADD_show[which(toupper(CADD$GeneName)%in%genes_in_gps()&CADD$PPA>=ppa()),]
			  if(any(duplicated(x))) x = x[-which(duplicated(x)),]
			  datatable(x, rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal)'),
						  htmltools::tags$p('Reference and alternative allele from human reference genome (GRCh37)')
						  
						)
						,
						class ='display',
						container = sketchcadd
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  #store excerpt of CADD table overlapping searched genes:
	  cadd_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()%in%c(11:13))){
			if(any(toupper(CADD$GeneName)%in%genes_in_gps())){
			  x = CADD[which(toupper(CADD$GeneName)%in%genes_in_gps()&CADD$PPA>=ppa()),]
			  if(any(duplicated(x))){
				l = which(duplicated(x))
				x = x[-l,]
			  } 
			  x
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  #render cadd text:
	  output$CADD_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(cadd_gene())){
			  paste(genes_in_gps()," contains at least one credible variant that is predicted deleterious or has a functional consequence for ",genes_in_gps() ,":",sep="")
		  }else{
			  paste(genes_in_gps()," does not contain a credible variant that is predicted deleterious or has a functional consequence for ",genes_in_gps(),".",sep="")
		  }
		}else{
		  if(is.data.frame(cadd_gene())){
			if(length(genes_in_gps())==1){
			  paste(genes_in_gps()," contains at least one credible variant that is predicted deleterious or has a functional consequence for ",genes_in_gps() ,":",sep="")
			}else{
			  if(length(unique(cadd_gene()[,15])==1)){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, ",length(unique(cadd_gene()[,15]))," gene contains at least one credible variant that is predicted deleterious or has a functional consequence for this gene:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, ",length(unique(cadd_gene()[,15]))," genes contain at least one credible variant that is predicted deleterious or has a functional consequence for the respective gene:",sep="")
			  }
			}
		  }else{
			paste("None of your ",length(genes_in_gps()), " searched genes included in the GPS contains at least one credible variant that is predicted deleterious or has a functional consequence for the respective gene.",sep="")
		  }
		}
	  })
	  
	  #Neptune & Susztak glomerulus:
	  output$glo_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==14)){
			if(any(toupper(glo_show$`Affected gene`)%in%genes_in_gps())){
			  datatable(glo_show[which(toupper(glo_show$`Affected gene`)%in%genes_in_gps() & glo_show$PPA>=ppa()),], rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						  htmltools::tags$p('** NEPTUNE, or Sheng et al, Nat.Genet. 2021')
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  glo_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==14)){
			if(any(toupper(glo$Approved.symbol)%in%genes_in_gps())){
			  glo[which(toupper(glo$Approved.symbol)%in%genes_in_gps() & glo$ppa>=ppa()),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$glo_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(glo_gene())){
			paste("Expression of ",genes_in_gps()," is modulated by a credible variant in glomerular tissue:",sep="")
		  }else{
			paste("No credible variant modulates expression of ",genes_in_gps()," in glomerular tissue.",sep="")
		  }
		}else{
		  if(is.data.frame(glo_gene())){
			if(length(genes_in_gps())==1){
			  paste("Expression of ",genes_in_gps()," is modulated by a credible variant in glomerular tissue:",sep="")
			}else{
			  if(length(unique(glo_gene()[,5]))==1){
			   paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(glo_gene()[,5]))," gene is modulated by a credible variant in glomerular tissue:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(glo_gene()[,5]))," genes is modulated by a credible variant in glomerular tissue:",sep="")
			  }
			}
			
		  }else{
			paste("Expression of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in glomerular tissue.",sep="")
		  }
		}
	  })
	  
	  #Neptune & Susztak tubulo-interstitium:
	  output$tub_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==15)){
			if(any(toupper(tub_show$`Affected gene`)%in%genes_in_gps())){
			  datatable(tub_show[which(toupper(tub_show$`Affected gene`)%in%genes_in_gps()&tub_show$PPA>=ppa()),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						  htmltools::tags$p('** NEPTUNE, or Sheng et al, Nat.Genet. 2021')
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  tub_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==15)){
			if(any(toupper(tub$Approved.symbol)%in%genes_in_gps())){
			  tub[which(toupper(tub$Approved.symbol)%in%genes_in_gps()&tub$ppa>=ppa()),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$tub_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(tub_gene())){
			paste("Expression of ",genes_in_gps()," is modulated by a credible variant in tubulo-interstitial tissue:",sep="")
		  }else{
			paste("No credible variant modulates expression of ",genes_in_gps()," in tubulo-interstitial tissue.",sep="")
		  }
		}else{
		  if(is.data.frame(tub_gene())){
			if(length(genes_in_gps())==1){
			  paste("Expression of ",genes_in_gps()," is modulated by a credible variant in tubulo-interstitial tissue:",sep="")
			}else{
			  if(length(unique(tub_gene()[,'Approved.symbol']))==1){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(tub_gene()[,'Approved.symbol']))," gene is modulated by a credible variant in tubulo-interstitial tissue:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(tub_gene()[,'Approved.symbol']))," genes is modulated by a credible variant in tubulo-interstitial tissue:",sep="")
			  }
			  
			}
			
		  }else{
			paste("Expression of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in tubulo-interstitial tissue.",sep="")
		  }
		}  
	  })
	  
	  #GTEx eQTL kidney
	  output$eqtl_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==16)){
			if(any(toupper(GTEx_eQTL$gene)%in%genes_in_gps())){
			  datatable(GTEx_eQTL_show[which(toupper(GTEx_eQTL$gene)%in%genes_in_gps()&GTEx_eQTL$ppa>=ppa()),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  eqtl_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==16)){
			if(any(toupper(GTEx_eQTL$gene)%in%genes_in_gps())){
			  GTEx_eQTL[which(toupper(GTEx_eQTL$gene)%in%genes_in_gps()&GTEx_eQTL$ppa>=ppa()),]
			  
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$eqtl_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(eqtl_gene())){
			paste("Expression of ",genes_in_gps()," is modulated by a credible variant in kidney cortex tissue:",sep="")
		  }else{
			paste("No credible variant modulates expression of ",genes_in_gps()," in kidney cortex tissue.",sep="")
		  }
		}else{
		  if(is.data.frame(eqtl_gene())){
			if(length(genes_in_gps())==1){
			  paste("Expression of ",genes_in_gps()," is modulated by a credible variant in kidney cortex tissue:",sep="")
			}else{
			  if(length(unique(eqtl_gene()[,'gene']))==1){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(eqtl_gene()[,'gene']))," gene is modulated by a credible variant in kidney cortex tissue:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(eqtl_gene()[,'gene']))," genes is modulated by a credible variant in kidney cortex tissue:",sep="")
			  }
			  
			}
			
		  }else{
			paste("Expression of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in kidney cortex tissue.",sep="")
		  }
		}
	  })
	  
	  #GTEx other tissue eQTL
	  
	  output$eqtl_wo_kidney_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==19)){
			if(any(toupper(gtex_without_kidney_show[,3])%in%genes_in_gps())){ "Affected gene"
			  datatable(gtex_without_kidney_show[which(toupper(gtex_without_kidney_show[,3])%in%genes_in_gps()& gtex_without_kidney_show$PPA >= ppa()),c(1:13)],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions='Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* eQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
			  )%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  eqtl_wo_kidney_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==19)){
			if(any(toupper(gtex_without_kidney_show[,3])%in%genes_in_gps())){ # column 3 is "Affected gene"
			  gtex_without_kidney_show[which(toupper(gtex_without_kidney_show[,3])%in%genes_in_gps()& gtex_without_kidney_show$PPA >= ppa()),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$eqtl_wo_kidney_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(eqtl_wo_kidney_gene())){
			paste("Expression of ",genes_in_gps()," is modulated by a credible variant in other tissues:",sep="")
		  }else{
			paste("No credible variant modulates expression of ",genes_in_gps()," in other tissues.",sep="")
		  }
		}else{
		  if(is.data.frame(eqtl_wo_kidney_gene())){
			if(length(genes_in_gps())==1){
			  paste("Expression of ",genes_in_gps()," is modulated by a credible variant in other tissues:",sep="")
			}else{
			  if(length(unique(eqtl_wo_kidney_gene()[,3]))==1){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(eqtl_wo_kidney_gene()[,3]))," gene is modulated by a credible variant in other tissues:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," expression of ",length(unique(eqtl_wo_kidney_gene()[,3]))," genes is modulated by a credible variant in other tissues:",sep="")
			  }
			  
			}
			
		  }else{
			paste("Expression of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in other tissues.",sep="")
		  }
		}
	  })
	  
	  
	  # GTEx sQTL kidney
	  output$sqtl_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==18)){
			if(any(toupper(GTEx_sQTL$gene)%in%genes_in_gps())){
			  datatable(GTEx_sQTL_show[which(toupper(GTEx_sQTL$gene)%in%genes_in_gps()& GTEx_sQTL$ppa >= ppa()),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions='Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* sQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
						)%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  
	  sqtl_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==18)){
			if(any(toupper(GTEx_sQTL$gene)%in%genes_in_gps())){
			  GTEx_sQTL[which(toupper(GTEx_sQTL$gene)%in%genes_in_gps()& GTEx_sQTL$ppa >= ppa()),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$sqtl_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(sqtl_gene())){
			paste("Splicing of ",genes_in_gps()," is modulated by a credible variant in kidney cortex tissue:",sep="")
		  }else{
			paste("No credible variant modulates splicing of ",genes_in_gps()," in kidney cortex tissue.",sep="")
		  }
		}else{
		  if(is.data.frame(sqtl_gene())){
			if(length(genes_in_gps())==1){
			  paste("Splicing of ",genes_in_gps()," is modulated by a credible variant in kidney cortex tissue:",sep="")
			}else{
			  if(length(unique(sqtl_gene()[,'gene']))==1){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," splicing of ",length(unique(sqtl_gene()[,'gene']))," gene is modulated by a credible variant in kidney cortex tissue:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," splicing of ",length(unique(sqtl_gene()[,'gene']))," genes is modulated by a credible variant in kidney cortex tissue:",sep="")
			  }
			}
			
		  }else{
			paste("Splicing of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in kidney cortex tissue.",sep="")
		  }
		}
	  })
	  
	  
	  
	  # GTEx sQTL other tissues
	  output$sqtl_wo_kidney_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==19)){
			if(any(toupper(sqtl_without_kidney_show[,3])%in%genes_in_gps())){ #column 3 is 'Affected gene'
			  datatable(sqtl_without_kidney_show[which(toupper(sqtl_without_kidney_show[,3])%in%genes_in_gps()& sqtl_without_kidney_show$PPA >= ppa()),c(1:13)],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions='Buttons',
						caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* sQTL effect allele is the eGFRcrea lowering allele'),
						  htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal), EAF: Effect allele frequency'),
						)
			  )%>% formatPercentage('PPA')
			}
		  }
		}
	  })
	  sqtl_wo_kidney_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==19)){
			if(any(toupper(sqtl_without_kidney_show[,3])%in%genes_in_gps())){ #column 3 is 'Affected gene'
			  sqtl_without_kidney_show[which(toupper(sqtl_without_kidney_show[,3])%in%genes_in_gps()& sqtl_without_kidney_show$PPA >= ppa()),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$sqtl_wo_kidney_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(sqtl_wo_kidney_gene())){
			paste("Splicing of ",genes_in_gps()," is modulated by a credible variant in other tissues:",sep="")
		  }else{
			paste("No credible variant modulates splicing of ",genes_in_gps()," in other tissues.",sep="")
		  }
		}else{
		  if(is.data.frame(sqtl_wo_kidney_gene())){
			if(length(genes_in_gps())==1){
			  paste("Splicing of ",genes_in_gps()," is modulated by a credible variant in other tissues:",sep="")
			}else{
			  if(length(unique(sqtl_wo_kidney_gene()[,3]))==1){
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," splicing of ",length(unique(sqtl_wo_kidney_gene()[,3]))," gene is modulated by a credible variant in other tissues:",sep="")
			  }else{
				paste("Of your ",length(genes_in_gps()), " searched genes included in the GPS, "," splicing of ",length(unique(sqtl_wo_kidney_gene()[,3]))," genes is modulated by a credible variant in other tissues:",sep="")
			  }
			  
			}
			
		  }else{
			paste("Splicing of none of your ",length(genes_in_gps()), " searched genes included in the GPS is modulated by a credible variant in other tissues.",sep="")
		  }
		}
	  })
	  
	  #MGI
	  output$mgi_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==20)){
			if(any(toupper(MGI$human_symbol)%in%genes_in_gps())){
			  datatable(MGI_show[which(toupper(MGI$human_symbol)%in%genes_in_gps()),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons'
						)
			}
		  }
		}
	  })
	  
	  mgi_gene<-reactive({
		if(valid_gene()){
		  if(any(columns()==20)){
			if(any(toupper(MGI$human_symbol)%in%genes_in_gps())){
			  (MGI_show[which(toupper(MGI$human_symbol)%in%genes_in_gps()),]
			  )
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$mgi_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(mgi_gene())){
			paste("Following kidney phenotypes in mice are described for ",genes_in_gps()," :",sep="")
		  }else{
			paste("No kidney phenotypes in mice were described for ",genes_in_gps()," .",sep="")
		  }
		}else{
		  if(is.data.frame(mgi_gene())){
			if(length(genes_in_gps())==1){
			  paste("Following kidney phenotypes in mice are described for ",genes_in_gps()," :",sep="")
			}else{
			  if(length(unique(mgi_gene()[,1]))==1){
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(mgi_gene()[,1]))," gene is described with kidney phenotypes in mice:",sep="")
			  }else{
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(mgi_gene()[,1]))," genes are described with kidney phenotypes in mice:",sep="")
			  }
			}
			
		  }else{
			paste("No kidney phenotypes in mice were described for any of your ",length(genes_in_gps()), " searched genes included in the GPS.",sep="")
		  }
		}
		
	  })
	  
	  #OMIM, Groopman, Wopperer
	  output$omim_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==21)){
			  if(any(toupper(OMIM$Gene)%in%genes_in_gps())){
			  datatable(OMIM_MIM[which(toupper(OMIM$Gene)%in%genes_in_gps()),c(1,4,2,3)],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						escape = FALSE
						,caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* Link to OMIM entry'),
						  htmltools::tags$p('** Online Mendelian Inheritance in Man, OMIM; Groopman et al, N.Engl.J.Med. 2019 or Wopperer et al, Kidney Int. 2022')
						)
						)
			}
		  }
		} 
	  })
	  
	  
	  
	  omim_gene <- reactive({
		if(valid_gene()){
		  if(any(columns()==21)){
			if(any(toupper(OMIM$Gene)%in%genes_in_gps())){
			  OMIM_MIM[which(toupper(OMIM$Gene)%in%genes_in_gps()),c(1,4,2,3)]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$omim_gene_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(omim_gene())){
			paste("Following genetic disorders with kidney phenotypes in human are described for ",genes_in_gps()," :",sep="")
		  }else{
			paste("No genetic disorders with kidney phenotypes in human were described for ",genes_in_gps()," .",sep="")
		  }
		}else{
		  if(is.data.frame(omim_gene())){
			if(length(genes_in_gps())==1){
			  paste("Following genetic disorders with kidney phenotypes in human are described for ",genes_in_gps()," :",sep="")
			}else{
			  if(length(unique(omim_gene()[,1]))==1){
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(omim_gene()[,1]))," gene is described with genetic disorders with kidney phenotypes in human:",sep="")
			  }else{
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(omim_gene()[,1]))," genes are described with genetic disorders with kidney phenotypes in human:",sep="")
			  }
			  
			}
			
		  }else{
			paste("No genetic disorders with kidney phenotypes in human were described for any of your ",length(genes_in_gps()), " searched genes included in the GPS.",sep="")
		  }
		}
		
	  })
	  
	  #drugability 
	  #kidney drugs
	  output$drug_gene_kid <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==25)){
			if(any(toupper(drugability$gps_gene[which(drugability$includes_kideny_disease)])%in%genes_in_gps())){
			  datatable(drugability_show[which(toupper(drugability$gps_gene)%in%genes_in_gps() & drugability$includes_kideny_disease),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						escape = FALSE
						,caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* Link to Therapeutic Target Database (TTD)'),
						  htmltools::tags$p('** This status is the highest status for any target of this drug. This does not necessarily need to be the queried gene. Check TTD for more information')
						  
						)
			  )
			}
		  }
		} 
	  })
	  
	  
	  
	  drug_gene_kid <- reactive({
		if(valid_gene()){
		  if(any(columns()==25)){
			if(any(toupper(drugability$gps_gene[which(drugability$includes_kideny_disease)])%in%genes_in_gps())){
			  drugability_show[which(toupper(drugability$gps_gene)%in%genes_in_gps() & drugability$includes_kideny_disease),]
			}else{0}
		  }else{0}
		}else{0}
	  })

	  output$drug_gene_kid_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(drug_gene_kid())){
			paste("Following drug information with indication for kidney diseases is described for ",genes_in_gps()," :",sep="")
		  }else{
			paste(genes_in_gps()," is not described as known drug target or to show a drug interaction for drugs with indication for kidney diseases.",sep="")
		  }
		}else{
		  if(is.data.frame(drug_gene_kid())){
			if(length(genes_in_gps())==1){
			  paste("Following drug information with indication for kidney diseases is described for ",genes_in_gps()," :",sep="")
			}else{
			  if(length(unique(drug_gene_kid()[,1]))==1){
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(drug_gene_kid()[,1]))," gene is described as a known drug target or to show a drug interaction for drugs with indication for kidney diseases:",sep="")
			  }else{
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(drug_gene_kid()[,1]))," genes are described as a known drug target or to show a drug interaction for drugs with indication for kidney diseases:",sep="")
			  }
			  
			}
			
		  }else{
			paste("None of your ",length(genes_in_gps()), " searched genes included in the GPS is described as a known drug target or to show a drug interaction for drugs with indication for kidney diseases.",sep="")
		  }
		}
		
	  })
	  
	  #other drugs
	  
	  output$drug_gene_oth <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(columns()==26)){
			if(any(toupper(drugability$gps_gene[which(!drugability$includes_kideny_disease)])%in%genes_in_gps())){
			  datatable(drugability_show[which(toupper(drugability$gps_gene)%in%genes_in_gps() & !drugability$includes_kideny_disease),],rownames=FALSE,
						options = list(
						  columnDefs = list(list(className = 'dt-left', targets = "_all"))
						  , scrollX=TRUE, scrollCollapse = TRUE,
						  dom = 'lfrtBip',
						  buttons= list(c('copy', 'excel','csv'))
						),
						extensions = 'Buttons',
						escape = FALSE
						,caption = htmltools::tags$caption(
						  style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						  htmltools::tags$p('* Link to Therapeutic Target Database (TTD)'),
						  htmltools::tags$p('** This status is the highest status for any target of this drug. This does not necessarily need to be the queried gene. Check TTD for more information')
						  
						)
			  )
			}
		  }
		} 
	  })
	  
	  
	  
	  drug_gene_oth <- reactive({
		if(valid_gene()){
		  if(any(columns()==26)){
			if(any(toupper(drugability$gps_gene[which(!drugability$includes_kideny_disease)])%in%genes_in_gps())){
			  drugability_show[which(toupper(drugability$gps_gene)%in%genes_in_gps() & !drugability$includes_kideny_disease),]
			}else{0}
		  }else{0}
		}else{0}
	  })
	  
	  output$drug_gene_oth_text <- renderText({
		if(inputtype$gene_final=="single"){
		  if(is.data.frame(drug_gene_oth())){
			paste("Following drug information with indication for other diseases or missing indication is described for ",genes_in_gps()," :",sep="")
		  }else{
			paste(genes_in_gps()," is not described as known drug target or to show a drug interaction for drugs with missing indication or indication for other diseases.",sep="")
		  }
		}else{
		  if(is.data.frame(drug_gene_oth())){
			if(length(genes_in_gps())==1){
			  paste("Following drug information with indication for other diseases or missing indication is described for ",genes_in_gps()," :",sep="")
			}else{
			  if(length(unique(drug_gene_oth()[,1]))==1){
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(drug_gene_oth()[,1]))," gene is described as a known drug target or to show a drug interaction for drugs with missing indication or indication for other diseases:",sep="")
			  }else{
				paste("Of your ", length(genes_in_gps())," searched genes included in the GPS, ",length(unique(drug_gene_oth()[,1]))," genes are described as a known drug target or to show a drug interaction for drugs with missing indication or indication for other diseases:",sep="")
			  }
			  
			}
			
		  }else{
			paste("None of your ",length(genes_in_gps()), " searched genes included in the GPS is described as a known drug target or to show a drug interaction for drugs with missing indication or indication for other diseases.",sep="")
		  }
		}
		
	  })
	  
	  
	  #credible variants
	  output$cred_var_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(detail_evidence_locus_gene()=="cred_var")){
			
			a=unique(GPS$locus_id[which(toupper(GPS$Gene)%in%genes_in_gps())]) # extract Locus IDs the searched genes belong to
			
			z=cred_var_show[which(cred_var$`Locus Id`%in%a &cred_var$ppa >= ppa()),] # reduce cred_var_show table to variants belonging to those loci
			
			snps=cred_var$rsid[which(cred_var$`Locus Id`%in%a &cred_var$ppa >= ppa())] # make vector with rsids
			#add functional consequence if there is any:
			eqtl=rep("-",length(snps))
			if(is.data.frame(tub_gene())){eqtl[which(snps%in%tub_gene()[,1])]="eQTL"} # column 1 is always rsid
			if(is.data.frame(glo_gene())){eqtl[which(snps%in%glo_gene()[,1])]="eQTL"}
			if(is.data.frame(eqtl_gene())){eqtl[which(snps%in%eqtl_gene()[,1])]="eQTL"}
			sqtl=rep("-",length(snps))
			if(is.data.frame(sqtl_gene())){sqtl[which(snps%in%sqtl_gene()[,1])]="sQTL"}
			cadd=rep("-",length(snps))
			if(is.data.frame(cadd_gene())){cadd[which(snps%in%cadd_gene()[,1])]="protein-relevant SNP"}
			
			z['functional.consequence'] = ifelse(eqtl=="-", ifelse(sqtl=="-", ifelse(cadd=="-", "-", cadd),ifelse(cadd=="-", sqtl, paste(sqtl,cadd,sep=", "))),ifelse(sqtl=="-", ifelse(cadd=="-", eqtl, cadd),ifelse(cadd=="-", paste(eqtl,sqtlsep=", "), paste(eqtl,sqtl,cadd, sep=", "))))
			z=z[,c(1:4,17,5:16)]
			
			
			#render the final table:
			datatable(z, rownames=FALSE,
					  options = list(
						columnDefs = list(list(className = 'dt-left', targets = "_all"))
						, scrollX=TRUE, scrollCollapse = TRUE,
						dom = 'lfrtBip',
						buttons= list(c('copy', 'excel','csv'))
					  ),
					  extensions = 'Buttons',
					  escape=FALSE,
					  caption = htmltools::tags$caption(
						style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						htmltools::tags$p('* Link to dbSNP'),
						htmltools::tags$p('PPA: posterior probability of association of the SNP (probability of driving the association of the respective signal)'),
						htmltools::tags$p('N: Sample size of European-ancestry GWAS meta-analysis for this SNP, Effect allele: eGFRcrea lowering allele, EAF: Effect allele frequency'),
						htmltools::tags$p('For loci with multiple independent signals, the association results are provided conditioned and unconditioned on the signal index variants of the other signals in the locus.')
					  ),
					  class='display',
					  container = sketchcredvargene
			)%>% formatPercentage('PPA')
		  }
		}
	  })
	  
	  #dm statistics
	  output$dm_stat_gene <- DT::renderDT(server=FALSE, {
		if(valid_gene()){
		  if(any(detail_evidence_locus_gene()=="dm_stat")){
			
			a=unique(GPS$locus_id[which(toupper(GPS$Gene)%in%genes_in_gps())])
			snps=cred_var$rsid[which(cred_var$`Locus Id`%in%a &cred_var$ppa >= ppa())]
			dm_gene = DM_stats_show[which(DM_stats$rsid%in%snps),]
			
			  
			
			
			datatable(dm_gene, rownames=FALSE,
					  options = list(
						columnDefs = list(list(className = 'dt-left', targets = "_all"))
						, scrollX=TRUE, scrollCollapse = TRUE,
						dom = 'lfrtBip',
						buttons= list(c('copy', 'excel','csv'))
					  ),
					  extensions = 'Buttons',
					  escape=FALSE,
					  caption = htmltools::tags$caption(
						style = 'caption-side: bottom; text-align: left; font-weight: normal;',
						htmltools::tags$p('N: Sample size of European-ancestry GWAS meta-analysis for this SNP, Effect allele: eGFRcrea lowering allele in the combined analysis, EAF: Effect allele frequency')
					  ),
					  class='display',
					  container = sketchdmstatgene
			)
		  }
		}
	  })
	  
	  #GPS
	  
	  #generate GPS table from all the above excerpts of functional tables (eQTL, sQTL, CADD)
	  GPS_gene <- reactive({
		if(valid_gene()){
		  
			
			x=GPS_show[which(toupper(GPS$Gene)%in%genes_in_gps()),]

			y=GPS[which(toupper(GPS$Gene)%in%genes_in_gps()),]
			rows=nrow(x)
			cadd1=rep(0,rows)
			cadd2=rep(0,rows)
			cadd3=rep(0,rows)
			tub_g=rep(0,rows)
			glo_g=rep(0,rows)
			eqtl_kid=rep(0,rows)
			sqtl_kid=rep(0,rows)
			eqtl_all=rep(0,rows)
			sqtl_all=rep(0,rows)
			for(i in 1:rows){
			  
			  if(is.data.frame(cadd_gene())){
				cadd1[i]=nrow(cadd_gene()[which(cadd_gene()['signal_id']==y[i,'signal_id'] & cadd_gene()['ConsScore']>6 & cadd_gene()['GeneName']==y[i,'Gene'] & cadd_gene()['region_id']==y[i,'locus_id']),]) # count how many variants per gene fullfil these criteria
				
				cadd2[i]=nrow(cadd_gene()[which(cadd_gene()['signal_id']==y[i,'signal_id'] & cadd_gene()['ConsScore']>4& cadd_gene()['ConsScore']<=6& cadd_gene()['GeneName']==y[i,'Gene'] & cadd_gene()['region_id']==y[i,'locus_id']),])
				
				cadd3[i]=nrow(cadd_gene()[which(cadd_gene()['signal_id']==y[i,'signal_id'] & cadd_gene()['ConsScore']<=4& cadd_gene()['GeneName']==y[i,'Gene'] & cadd_gene()['region_id']==y[i,'locus_id']),])
			  }
			  if(is.data.frame(tub_gene())){
				
				tub_g[i]=length(unique(tub_gene()[which(tub_gene()['signal_id']==y[i,'signal_id']& tub_gene()['Approved.symbol']==y[i,'Gene'] & tub_gene()['region_id']==y[i,'locus_id']),1]))
			  }
			  if(is.data.frame(glo_gene())){
				
				glo_g[i]=length(unique(glo_gene()[which(glo_gene()['signal_id']==y[i,'signal_id']& glo_gene()['Approved.symbol']==y[i,'Gene'] & glo_gene()['region_id']==y[i,'locus_id']),1]))
			  }
			  if(is.data.frame(eqtl_gene())){
				eqtl_kid[i]=nrow(eqtl_gene()[which(eqtl_gene()['signal_id']==y[i,'signal_id']& eqtl_gene()['gene']==y[i,'Gene'] & eqtl_gene()['region_id']==y[i,'locus_id']),])
			  }
			  if(is.data.frame(eqtl_wo_kidney_gene())){
				eqtl_all[i]=length(unique(eqtl_wo_kidney_gene()[which(eqtl_wo_kidney_gene()['signal_id']==y[i,'signal_id']& eqtl_wo_kidney_gene()['Affected gene']==y[i,'Gene'] & eqtl_wo_kidney_gene()['region_id']==y[i,'locus_id']),1]))
			  }
			  if(is.data.frame(sqtl_gene())){
				sqtl_kid[i]=nrow(sqtl_gene()[which(sqtl_gene()['signal_id']==y[i,'signal_id']& sqtl_gene()['gene']==y[i,'Gene'] & sqtl_gene()['region_id']==y[i,'locus_id']),])
			  }
			  if(is.data.frame(sqtl_wo_kidney_gene())){
				sqtl_all[i]=length(unique(sqtl_wo_kidney_gene()[which(sqtl_wo_kidney_gene()['signal_id']==y[i,'signal_id']& sqtl_wo_kidney_gene()['Affected gene']==y[i,'Gene'] & sqtl_wo_kidney_gene()['region_id']==y[i,'locus_id']),1]))
			  }
			}
			x[,'stop-gained, stop-lost, non-synonymus']=cadd1
			x[,'canonical splice, noncoding change, synonymous, splice site']=cadd2
			x[,'other deleterious variant']=cadd3
			x[,'eQTL glomerulus (NEPTUNE, or Sheng et al [Nat Genet, 2021])']=glo_g
			x[,'eQTL tubulo-interstitium (NEPTUNE, or Sheng et al [Nat Genet, 2021])']=tub_g
			x[,'eQTL kidney cortex (GTEx)']=eqtl_kid
			x[,'eQTL other tissue (GTEx)']=eqtl_all
			x[,'sQTL kidney cortex (GTEx)']=sqtl_kid
			x[,'sQTL other tissue (GTEx)']=sqtl_all
			
			x=x[,c('Gene*','Locus name**','Locus ID','Signal ID','eGFRcys or BUN validation','DM_effect','decline_effect','# credible variants in signal','max PPA',names(GPS_show)[columns()],'Distance to locus lead variant','Chromosome','Position gene start','Position gene end')]
			#names(x)=c("Locus name**","Locus ID", "Signal ID", "Gene*", "Distance to locus lead variant","Chromosome","Start of gene","End of gene", "eGFRcys or BUN validation", "# credible variants in signal","max PPA","stop-gained, stop-lost, non-synonymus","canonical splice, noncoding change, synonymous, splice site","other deleterious variant","eQTL glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL kidney cortex tissue (GTEx)","eQTL other tissue (GTEx)","sQTL kidney cortex tisse (GTEx)","sQTL other tissue (GTEx)","mouse phenotype","human phenotype", "Coloc in NEPTUNE tissue","Diabetes specific effect","eGFRcrea decline effect","Drugable","In enriched pathway")
			#x[,c(4,1:3,9,25,26,10,11,(columns()+1),24:27,5:8)] #max PPA column is new -> alternativly change column values everywhere
			
		  
		}else{0}
	  })
	  
	  
	  # count how long the GPS table is:
	  rowsGPS <- reactive({
		if(valid_gene()){nrow(GPS_gene())}
		else{0}
	  })
	  
	  #header GPS
	  sketch_gene <- reactive({
		
		htmltools::withTags((table(
		  class='display',
		  thead(
			tr(
			  th(rowspan = 2, class="Geneofficial hover", 'Gene*', span(class="Genecomment", "Official gene name from HGNC")),
			  th(rowspan = 2, 'Locus name**'),
			  th(rowspan = 2, class="Locusidwithcomment hover",'Locus ID', span(class="Locusidcomment", "k: known locus from Wuttke et al. Nat.commun.2019, n: novel locus in Stanzick et al. Nat.commun. 2021")),
			  th(rowspan = 2, class="Signalidwithcomment hover",'Signal ID', span(class="Signalidcomment", "ID of independent signals within a locus")),
			  th(rowspan = 2, class="validationwithcomment hover",'eGFRcys or BUN validation', span(class="validationcomment",list( "Information whether the locus lead variant is nominal significant (P<0.05) associated with concordent effect direction with ", tags$abbr(title="glomerular filtration rate estimated from serum cystatin C","eGFRcys"),"or ",tags$abbr(title="blood urea nitrogen","BUN")))),
			  th(rowspan = 2, class="DMwithcomment hover",'Signal association depends on DM', span(class="DMcomment",list( "Information whether the signal index variant (or a correlated variant) shows significant interaction with ", tags$abbr(title="Diabetes mellitus","DM"), "-status, Winkler et al. 2022"))),
			  th(rowspan = 2, class="declinewithcomment hover",'Signal association with eGFRcrea decline', span(class="declinecomment",list( "Information whether the signal index variant (or a correlated variant) was established with eGFR-decline in Gorski et al. 2022 or Wiegrebe et al. 2024"))),
			  th(rowspan = 2, '# credible variants in signal'),
			  th(rowspan = 2, class="maxPPA hover",'max PPA', span(class="maxPPAcomment", "max. probability of a credible variant in this signal to be causal")),
			  if(any(columns()%in%c(11:13))) th(colspan = length(which(columns()%in%c(11:13))), class="CADD", span('# protein-relevant credible variants in the gene')),
			  if(any(columns()%in%c(14:19))) th(colspan = length(which(columns()%in%c(14:19))), class="eqtl", '# credible variants that modulate gene expression (eQTLs) or splicing (sQTLs), FDR < 5%'),
			  if(any(columns()==20)) th(rowspan = 2, class="eqtl", 'Coloc in NEPTUNE tissue'),
			  if(any(columns()==21)) th(rowspan = 2, class="pheno",'# kidney phenotypes in mouse'),
			  if(any(columns()==22)) th(rowspan = 2, class="pheno", '# kidney phenotypes in human'),
			  if(any(columns()%in%c(25,26))) th(rowspan = 2, class="drug", 'Gene is known as drug target or for drug interaction'),
			  th(rowspan = 2, 'Distance to locus lead variant'),
			  th(rowspan = 2, 'Chromosome'),
			  th(rowspan = 2, 'Position gene start'),
			  th(rowspan = 2, 'Position gene end')
			),
			tr(
			  if(any(columns()==11)) th(class="CADD",'stop-gained, stop-lost, non-synonymous'),
			  if(any(columns()==12)) th(class="CADD",'canonical splice, noncoding change, synonymous, splice site'),
			  if(any(columns()==13)) th(class="CADD", span('other deleterious variant (CADD-Phred',HTML('<span>&#8805;</span>'),'15)')),
			  if(any(columns()==14)) th(class='eQTLglowithcomment hover eqtl', 'eQTL glomerulus', span(class='eQTLglocomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			  if(any(columns()==15)) th(class='eQTLtubwithcomment hover eqtl','eQTL tubulo-interstitium', span(class='eQTLtubcomment','NEPTUNE v2, or Sheng et al. [Nat.Genet. 2021]')),
			  if(any(columns()==16)) th(class="eqtl",'eQTL kidney cortex (GTEx)'),
			  if(any(columns()==17)) th(class="eqtl",'eQTL other tissue (GTEx)'),
			  if(any(columns()==18)) th(class="eqtl",'sQTL kidney cortex (GTEx)'),
			  if(any(columns()==19)) th(class="eqtl",'sQTL other tissue (GTEx)')
			)
		  )
		)))
		
	   })
	  
	  
	  # render GPS table:
	  output$GPSrows <-  DT::renderDT( server =FALSE, {
		
		
		datatable(GPS_gene(), rownames=FALSE,
				  options = list(
					columnDefs = list(list(className = 'dt-left', targets = "_all"))
					, scrollX=TRUE, scrollCollapse = TRUE,
					lengthChange=TRUE,
					dom = 'lfrtBip',
					#buttons = list(extend = "csv", text = "Download Current Page", filename = "page",
								   #exportOptions = list(
									 #modifier = list(page = "current")))
					buttons= list(c('copy', 'excel','csv'))
				  ),
				  extensions = 'Buttons',
				  escape=FALSE,
				  caption = htmltools::tags$caption(
					style = 'caption-side: bottom; text-align: left; font-weight: normal;',
					'* Link to GeneCards, **nearest gene to locus lead-variant'),
				   container=sketch_gene()
		)%>% formatPercentage('max PPA')
	  })
	  
	  #Locus summary
	  output$summary <- renderDataTable({
			if(valid_gene()){
			  if(any(detail_evidence_locus_gene()=="summary")){
				a=unique(GPS$locus_id[which(toupper(GPS$Gene)%in%genes_in_gps())])
				b=region_table_show[which(region_table$Locus_id%in%a),]
				datatable(b[order(b[,1],b[,2]),], rownames=F,
						  options = list(
							columnDefs = list(list(className = 'dt-left', targets = "_all"))
							, scrollX=TRUE, scrollCollapse = TRUE,
							dom = 'lfrtBip',
							buttons= list(c('copy', 'excel','csv'))),
						  extensions = 'Buttons',
						  caption = htmltools::tags$caption(
							style = 'caption-side: bottom; text-align: left; font-weight: normal;',
							'* SNPs, which are associated with log(eGFRcrea) in the all-ancestry GWAS meta-analysis (unconditioned) with P<5E-8')
						  )
			  }
			}
	   })
	  
	  #Locus Zoom
	  Locus_ID<-reactive({
		# if(inputtype$gene_final=="single")
		if(valid_gene())
		{GPS$locus_id[which(GPS$Gene%in%genes_in_gps())]}
	  })
	  # filename1 <- reactive({
	  #   if(valid_gene()&inputtype$gene_final=="single"){
	  #     if(any(input$gene.locuszoom=="locus_zoom")){
	  #       as.character(lz_files[which(locus_id_files==as.character(Locus_ID()))][1])
	  #       
	  #     }
	  #   }
	  # })
	  # 
	  # output$LocusZoom <- renderUI({
	  #   req(filename1())
	  #   
	  #   tags$img(id="GPS_image",
	  #              height="100%",
	  #            width="100%",
	  #            src=filename1())
	  # })
	  
	  #create HTML syntax for link to plots
	  gene_plots <- reactive({
		if(valid_gene()){
		  ids=which(links_id%in%Locus_ID())[1] ## preliminary only one plot
		  paste("<p><a href=\"",links[ids],"\"target=\"_blank\">Regional association plot of locus",links_id[ids],"</a></p>")
		}
	  })
	  
	  #render links
	  output$plot_download_links_gene <- renderUI({
		list(lapply(gene_plots(),HTML))
	  })
	  
	  # program general gene-search help window:
	  observeEvent(input$question_GPS_gene,{
		showModal(modalDialog(
		  titel="GPS table",
		  tags$h4("GPS table help:"),
		  p("Enter a gene name and choose every evidence you want to see in the GPS table. Numbers within the CADD-, eQTL- and sQTL-columns display the number of credible variants in the association signal with the respective characteristics. Numbers in the MGI and OMIM column represent the number of different phenotypes/diseases found for the respective gene. Fur further information check the Documentation page."),
		  easyClose = TRUE,
		  footer=NULL,
		  size = "m"
		))
	  })
	  
	  # program gene-search further options help window:
	  observeEvent(input$question_details_gene,{
		showModal(modalDialog(
		  titel="Details GPS Table",
		  tags$h4("Details on GPS table - Help:"),
		  p("Click here for details on the entries you find in the GPS table for a gene. If a gene has no entry in the respective column the selected option will not give you any result. For further information check the Documentation page."),
		  easyClose = TRUE,
		  footer=NULL,
		  size = "m"
		))
	  })
	  
	  #show relevant sections:
	  observe({
		
		shinyjs::toggleElement(id="GPS_gene", condition = (rowsGPS()!=0) ) # show results section when there is at least one searched gene that is included in KidneyGPS
		
	  })
	  observeEvent(go$gene,{shinyjs::showElement(id="Gene_div")})
	  observeEvent(go$gene,{runjs('
		  document.getElementById("Gene_div").scrollIntoView({ left: 0, block: "end", behavior: "smooth" });
		')})
	  
	  # show the other sections when they are selected:
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_cadd_gene", condition= (any(columns()%in%c(11:13))&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_glo_gene", condition= (any(columns()==14)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_tub_gene", condition= (any(columns()==15)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_GTEx_eqtl_gene", condition= (any(columns()==16)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_GTEx_sqtl_gene", condition= (any(columns()==18)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_GTEx_wo_kidney_eqtl_gene", condition= (any(columns()==17)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_GTEx_wo_kidney_sqtl_gene", condition= (any(columns()==19)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_mgi_gene", condition= (any(columns()==20)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_omim_gene", condition= (any(columns()==21)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_drug_gene_kid", condition= (any(columns()==25)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_drug_gene_oth", condition= (any(columns()==26)&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_summary", condition= any(detail_evidence_locus_gene()=="summary"&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_cred_var_gene", condition= any(detail_evidence_locus_gene()=="cred_var"&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_dm_stat_gene", condition= any(detail_evidence_locus_gene()=="dm_stat"&valid_gene()))})
	  observeEvent(go$gene,{shinyjs::toggleElement(id="div_locus_zoom_gene", condition= any(input$gene.locuszoom=="locus_zoom"&valid_gene()))})
	  
	  
	}

