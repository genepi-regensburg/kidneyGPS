##########################
#
### Design User Interface
#
# Mathias Gorski, Feb 2025
#
# ##########################
# source("data/packages.r")

	ui <- 
	  fluidPage(# whole KidneyGPS is a fluid page
		tags$head( # header which loads the css file and defines the function reacting on the "Enter" key; this event is sent to the server
		  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
		  tags$script('
											$(document).on("keyup", function(e) {
											if(e.keyCode == 13){
											Shiny.onInputChange("keyPressed", Math.random());
											}
											});
							  ')
		),
	  
	 ################################################### header section
	  
	  title="KidneyGPS", #Name that is shown in the Browser Tab
	  #main header including the kidney image:
	  tags$h1(list(tags$img(id="kidney_image",
					   height="50px",
					   width="50px",
					   src= "kidney-g8a69f0709_1920.png"),
			  HTML(paste(tags$em("Kidney "), tags$b("G"),"ene",tags$b("P"),"rioriti",tags$b("S"),"ation - ",tags$b("KidneyGPS"),sep="")))), # tags$b: bold, tags$em: emphasized
	  tags$p(list("This platform summarizes information on each of 5906 genes overlapping the 424 loci identified for ",
				  tags$abbr(title="estimated glomerular filtration rate from serum creatinine",tags$b("eGFRcrea")) # example of abbreviations that will be shown, when hovered
				  , " based on a ",tags$abbr(title="genome-wide association studies","GWAS")," meta-analysis of ", tags$a(href="https://www.ukbiobank.ac.uk/", target="_blank", "UK Biobank data") # tags$a: format link.
				  , "and ", tags$a(href="https://ckdgen.imbi.uni-freiburg.de", target="_blank", "CKDGen consortium"), "data", tags$b("(n=1,201,909)"), 
			 tags$strong(tags$a(href="https://www.nature.com/articles/s41467-021-24491-0#Sec31", target="_blank", "[Stanzick et al. Nat. Commun. 2021]")),
			 ". Focussed on European ancestry ",tags$b("(n=1,004,040)"),", 594 independent signals across the 424 loci were identified using approximate conditioned analysis. "
			 , "For each variant in these 594 signals, the posterior probability of association (PPA) was computed and, for each signal, a 99% credible set of variants was derived "
			 ,"(i.e. smallest set of variants with >99% cumultative PPA). ",
			 "Credible (set) variants are considered the most likely variants to drive the association signal (particularly those with high PPA).")),
	  
	   
	   ################################################### Tabs section
	  # From here definition of the 5 Tabs inside KidneyGPS:
	  
	  navbarPage(tags$p(HTML(paste(tags$b("G"),"ene",tags$b("P"),"rioriti",tags$b("S"),"ation", sep=""))),
				 
				 
				 
				 tabPanel("Genes", #Every Tab starts directly with the name of the Tab
						  
						  tags$h3("Gene Search"), #header of category 3
						  p("Search for genes overlapping any of the 424 loci:"), # p: paragraph
						  
						  sidebarLayout(# this layout allows for one sidebar Panel and one main Panel.

						   # Sidebar panel for inputs ----
						   sidebarPanel(
							textInput(inputId="Genename", label="Gene Name", value = ""), # single gene input
							# tags$h4(actionButton(inputId="question_gene_batch",label="?",class="question"),"Upload Gene list:"),
							tags$br(), # line break
							
							div(class="gene-batch",textAreaInput("gene_batch",label="Paste a list of genes",value=NULL)), # multiple gene input

							tags$hr(),# Horizontal line ----
							# Input: Select a file ----
							fluidRow( #each fluid row can have multiple columns with a total width of 12 
								column(12, #only one column with the full width of the fluid row, which can be as wide as the sidebar panel
									   div(id="gene-upload-section",
										   fileInput("file2", "Choose txt file with a list of genes (max. 2000)", #input file 
										   multiple = FALSE,
										   accept = c("text/txt",
													  "text/comma-separated-values,text/plain",
													  ".txt")),
									   actionButton(inputId="reset_gene", label="reset", class="go"))
									   ),
							 ),

							 tags$hr(), # Horizontal line ----
							 shinyjs::hidden( # the following section is hidden until a file is uploaded. This behavior is controlled in the server part
							   div(id="gene_batch_upload_options",
							   # Input: Checkbox if file has header ---- not reqired if just a list should be uploaded
							   #checkboxInput("header_gene", "Header", TRUE),

							   # Input: Select separator ----
								   radioButtons("sep_gene", "Separator",
												choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),

							   # # Input: Select quotes ----
							   #     radioButtons("quote_gene", "Quote",
							   #                  choices = c(None = "",
							   #                              "Double Quote" = '"',
							   #                              "Single Quote" = "'"),
							   #                  selected = '"')

							   )),
							fluidRow(
							  column(2, offset=10, # column of width 2 with offset of 10 to locate it at the right 
									 shinyjs::hidden(div(id="gene_go_div",actionButton(inputId="gene_go", label="go", class="go"))), # The go button will be shown if there is input in any of the input fields
							  )
							)
						   ),
						   
						   mainPanel( # main Panel of the sidebarLayout
							 wellPanel( # well Panels have a specific format so that they look nice
							   
							   
							   
							   
							   tags$h4("Specification of functional evidence for the searched gene (s) displayed in GPS table:",actionButton(inputId="question_GPS_gene",label="?",class="question")),
							   
							   
							   div(class="CADD",checkboxGroupInput(inputId="columns1", label=span("Gene contains credible set variant that is protein-relevant"),  selected = c(11:13), # preselection of options by using the choiceValues
																   inline = FALSE, width = NULL, choiceNames = c("stop-gained, stop-lost, non-synonymus","canonical splice, noncoding change, synonymous, splice site","other functional consequence with high predicted deleteriousness (CADD-Phred \u2265 15)"), choiceValues = c(11:13))), # I used these values as these are the respective columns of the GPS table.
							   
							   div(class="eqtl",checkboxGroupInput(inputId="columns2", label="Gene maps to credible variant that modulates gene expression (eQTLs) or splicing (sQTLs), FDR < 5%",  selected = c(14:19,22),
																   inline = FALSE, width = NULL, choiceNames = c("eQTL in glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in kidney cortex tissue (GTEx)","eQTL in other tissue (GTEx)","sQTL in kidney cortex tissue (GTEx)","sQTL in other tissue (GTEx)", "Colocalization of gene-expression signal and eGFRcrea signal (NEPTUNE)"), choiceValues = c(14:19,22))),
							   
							   div(class="pheno",checkboxGroupInput(inputId="columns3", label="Gene has known kidney phenotype in mouse or human:",  selected = c(20,21),
																	inline = FALSE, width = NULL, choiceNames = c("mouse (Mouse Genome Informatics, MGI)","human (online mendelian inheritance in man, OMIM, Groopman et al. [N.Engl.J.Med,2019], or Wopperer et al. [Kidney Int.,2022])"), choiceValues = c(20,21))),
							   div(class="drug",checkboxGroupInput(inputId="columns4", label="Gene is known as drug target or to show a drug interaction:",  selected = c(25), #test with 26 for other indications
																	inline = FALSE, width = NULL, choiceNames = c("Drug information (kidney diseases)","Drug information (other or missing indication)" ), choiceValues = c(25,26))),
							   tags$br(),
							   tags$h4("Specification of additional information",actionButton(inputId="question_details_gene",label="?",class="question")),
							   
							   
							   checkboxGroupInput(inputId="detail_evidence_locus_gene", label=NULL,  selected = NULL,
												  inline = FALSE, width = NULL, choiceNames = c("Locus information for loci containing the searched genes", "eGFR association statistics for all credible set variants in the locus","eGFR association statistics for all credible set variants in the locus separated by diabetes status"), choiceValues = c("summary","cred_var","dm_stat")),
							   
							   div(class="genelocuszoom",checkboxGroupInput(inputId="gene.locuszoom", label=NULL,  selected = NULL,
																			  inline = FALSE, width = NULL, choiceNames = c("Regional association plot of the eGFRcrea locus containing the searched gene (LocusZoom)"), choiceValues = c("locus_zoom")),
									 
							   ),
							   
							   tags$br(),
							   tags$h5(tags$b("Restrict results to credible set variants with a minimum PPA* of:")),
							   numericInput(inputId="ppa", label=NULL, value =0, min=0, max=1, width="20%"), # Input for PPA restriction 
							   tags$p("*PPA: posterior probability of the eGFRcrea association  (max. PPA 1.0)"),
							   
							   
							   
							 )            
						  )
						  ),
						  
						  
						  
						  useShinyjs(),
						  
						  shinyjs::hidden(  # here starts the results section, which is hidden as long as no search is started      
							div(id="Gene_div", 
								tags$p(textOutput("testforme")),
								tags$p(textOutput("namecheck_gene")),
								
								
								shinyjs::hidden( # this is the section of the GPS. It is only shown when the searched gene is included in the GPS Table
								  div( id ="GPS_gene",
									   tags$h4("GPS Table", class="Output_header"),
									   DT::dataTableOutput("GPSrows"),
									   br(),
									  # downloadButton("downloadGPS", "Download"),
								  )
								),
								hr(),
								
								#textOutput("extras_gene"),
								
								# all these other section will be shown if there is content and the respective option is selected.
								shinyjs::hidden(div(id="div_cadd_gene",tags$h4(span("Protein-relevant variants with predicted deleteriousness:"), class="Output_header"),
													textOutput("CADD_gene_text"),
													br(),
													DT::dataTableOutput("CADD_gene"),
													hr())),
								shinyjs::hidden(div(id="div_glo_gene",tags$h4("eQTLs in glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021]):", class="Output_header"),
													textOutput("glo_gene_text"),
													br(),
													DT::dataTableOutput("glo_gene"),
													hr())),
								shinyjs::hidden(div(id="div_tub_gene",tags$h4("eQTLs in tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021]):", class="Output_header"),
													textOutput("tub_gene_text"),
													br(),
													DT::dataTableOutput("tub_gene"),
													hr())),
								shinyjs::hidden(div(id="div_GTEx_eqtl_gene",tags$h4("eQTLs in kidney-cortex tissue (GTEx):", class="Output_header"),
													textOutput("eqtl_gene_text"),
													br(),
													DT::dataTableOutput("eqtl_gene"),
													hr())),
								shinyjs::hidden(div(id="div_GTEx_sqtl_gene",tags$h4("sQTLs in kidney-cortex tissue (GTEx):", class="Output_header"),
													textOutput("sqtl_gene_text"),
													br(),
													DT::dataTableOutput("sqtl_gene"),
													hr())),
								shinyjs::hidden(div(id="div_GTEx_wo_kidney_eqtl_gene",tags$h4("eQTLs in other tissues (GTEx):", class="Output_header"),
													textOutput("eqtl_wo_kidney_gene_text"),
													br(),
													DT::dataTableOutput("eqtl_wo_kidney_gene"),
													hr())),
								shinyjs::hidden(div(id="div_GTEx_wo_kidney_sqtl_gene",tags$h4("sQTLs in other tissues (GTEx):", class="Output_header"),
													textOutput("sqtl_wo_kidney_gene_text"),
													br(),
													DT::dataTableOutput("sqtl_wo_kidney_gene"),
													hr())),
								shinyjs::hidden(div(id="div_mgi_gene",tags$h4("Kidney phenotypes in mouse:", class="Output_header"),
													textOutput("mgi_gene_text"),
													br(),
													DT::dataTableOutput("mgi_gene"),
													hr())),
								shinyjs::hidden(div(id="div_omim_gene",tags$h4("Kidney phenotypes in human:", class="Output_header"),
													textOutput("omim_gene_text"),
													br(),
													DT::dataTableOutput("omim_gene"),
													hr())),
								shinyjs::hidden(div(id="div_drug_gene_kid",tags$h4("Drugability or Interaction (indication for kidney diseases):", class="Output_header"),
													textOutput("drug_gene_kid_text"),
													br(),
													DT::dataTableOutput("drug_gene_kid"),
													hr())),
								shinyjs::hidden(div(id="div_drug_gene_oth",tags$h4("Drugability or Interaction (other indications):", class="Output_header"),
													textOutput("drug_gene_oth_text"),
													br(),
													DT::dataTableOutput("drug_gene_oth"),
													hr())),
								shinyjs::hidden(div(id="div_summary",tags$h4("Summary of loci and signals containing the searched gene(s):", class="Output_header"),
													textOutput("summary_text"),
													br(),
													DT::dataTableOutput("summary"),
													hr())),
								shinyjs::hidden(div(id="div_cred_var_gene",tags$h4("List of all credible variants in the locus containing the searched gene:", class="Output_header"),
													p("Shown are results from GWAS meta-analyses in European ancestry."),
													br(),
													DT::dataTableOutput("cred_var_gene"),
													hr())),
								shinyjs::hidden(div(id="div_dm_stat_gene",tags$h4("eGFR association statistics separated by diabetes status for all credible variants in the locus containing the searched gene:", class="Output_header"),
													p("Shown are results from GWAS meta-analyses in European ancestry."),
													br(),
													DT::dataTableOutput("dm_stat_gene"),
													hr())),
								shinyjs::hidden(div(id="div_locus_zoom_gene",tags$h4("Locus Zoom Plot:", class="Output_header"),
													textOutput("LocusZoom_gene_text"),
													uiOutput("plot_download_links_gene"),
													
													)),
								
								
							)),
				 ),
				 tabPanel("Variants", #here starts the next tab
						  p(tags$h3("SNP Search",actionButton(inputId="question_SNP",label="?",class="question"),style="display:inline")),
						  p("Search for SNPs which are associated with log(eGFRcrea) at P<5E-8 (all ancestries, unconditioned, n=1,201,909) or for SNPs being a credible variant for any of the 594 signals (European ancestry, conditioned, n=1,004,040):"),
						  useShinyjs(),
						  sidebarLayout(
						  
						  sidebarPanel(
							
							textInput(inputId="snp", label="Single SNP search", value=NULL, placeholder = "rs12345 | chr:pos"),
							# tags$h4(actionButton(inputId="question_SNP_batch",label="?",class="question"),"Upload SNP list:"),
							# tags$br(),
							tags$br(),
							
							div(class="SNP-batch", textAreaInput("SNP_batch",label="Paste a list of RSIDs",value=NULL, placeholder= "rs1234, rs4567, chr:pos")),
							
							tags$hr(),
							# Input: Select a file ----
							fluidRow(
							  
							  column(12,
									 div(id="snp-upload-section",
									 fileInput("file1", "Choose txt file with a list of RSIDs (max. 2000)",
											   multiple = FALSE,
											   accept = c("text/txt",
														  "text/comma-separated-values,text/plain",
														  ".txt")),
									 actionButton(inputId="reset_variant", label="reset", class="go")
							  ))
							  ,
							),
							# Horizontal line ----
							tags$hr(),
							shinyjs::hidden(
							  div(id="SNP_batch_upload_options",
								  # Input: Checkbox if file has header ----not reqired if just a list should be uploaded
								  #checkboxInput("header", "Header", TRUE),
								  
								  # Input: Select separator ----
								  radioButtons("sep_snp", "Separator",
											   choices = c(Comma = ",",Semicolon = ";",Tab = "\t",Space=" "),selected = ","),
								  
								  # Input: Select quotes ----
								  # radioButtons("quote_snp", "Quote",
								  #              choices = c(None = "",
								  #                          "Double Quote" = '"',
								  #                          "Single Quote" = "'"),
								  #              selected = '"'),
								  
								  
								  
							  )
							),
							fluidRow(
							  column(12,shinyjs::hidden(div(id="snp_go_div",actionButton(inputId="snp_go", label="go", class="go"))))
							),
							
						  
						  ),
						  
						  mainPanel(
						  
							wellPanel(
							  div(class="snp-search-options",
							  h4("Search options:"),
							
							  div(class="gwscredinputbox",checkboxGroupInput(inputId="variant-search-options", label=NULL,  selected = c("gws","credvar"),
																	   inline = FALSE, width = NULL, choiceNames = c("search for SNPs at P<5E-8", "search for credible variants"), choiceValues = c("gws","credvar")),
							  ),
							  uiOutput("detail.evidence.snp"), #uiOutput is a section of UI that will be generated in the server part. Here additional options get available when "credvar" is selected. Otherwise these options are disabled.
							  
							  
							  
							  div(class="snplocuszoom",checkboxGroupInput(inputId="snp.locuszoom", label="Specification of locus-based information",  selected = NULL,
																		  inline = FALSE, width = NULL, choiceNames = c("Regional association plot of the eGFRcrea locus containing the searched SNP"), choiceValues = c("locus_zoom")),
								  
							  ),
							  
							))
						  )
						  ),
						  
						  shinyjs::hidden(
							div(id="SNP_div",
								textOutput("snpinputcheck"),
								div(id="gws_div",tags$h4("eGFRcrea Association (P<5E-8 in all-ancestry GWAS meta-analyis, unconditioned):", class="Output_header"),
								textOutput("namecheck"),
								DT::dataTableOutput("all_var"),
								tags$hr()),
								div(id="cred_var_div",tags$h4("Credible variant(s):", class="Output_header"),   
								textOutput("namecheck2"),
								DT::dataTableOutput("cred_var"),
								tags$hr()),
								shinyjs::hidden(div(id="div_cadd_snp",tags$h4(span("Protein-relevance and predicted deleteriousness:"), class="Output_header"),
													#textOutput("extras"),
													textOutput("CADD_text"),
													DT::dataTableOutput("CADD"),
													tags$hr())),
								shinyjs::hidden(div(id="div_glo_snp",tags$h4("eQTL in glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021]):", class="Output_header"),
													textOutput("glo_text"),
													DT::dataTableOutput("glo"),
													tags$hr())),
								shinyjs::hidden(div(id="div_tub_snp",tags$h4("eQTL in tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021]):", class="Output_header"),
													textOutput("tub_text"),
													DT::dataTableOutput("tub"),
													tags$hr())),
								shinyjs::hidden(div(id="div_GTEx_eqtl_snp",tags$h4("eQTL in kidney-cortex tissue (GTEx):", class="Output_header"),
													textOutput("eqtl_text"),
													DT::dataTableOutput("eqtl"),
													tags$hr())),
								shinyjs::hidden(div(id="div_GTEx_sqtl_snp",tags$h4("sQTL in kidney-cortex tissue (GTEx):", class="Output_header"),
													textOutput("sqtl_text"),
													DT::dataTableOutput("sqtl"),
													tags$hr())),
								shinyjs::hidden(div(id="div_GTEx_eqtl_wo_kidney_snp",tags$h4("eQTL in other tissues (GTEx):", class="Output_header"),
													textOutput("eqtl_wo_kidney_text"),
													DT::dataTableOutput("eqtl_wo_kidney"),
													tags$hr())),
								shinyjs::hidden(div(id="div_GTEx_sqtl_wo_kidney_snp",tags$h4("sQTL in other tissues (GTEx):", class="Output_header"),
													textOutput("sqtl_wo_kidney_text"),
													DT::dataTableOutput("sqtl_wo_kidney"),
													tags$hr())),
								shinyjs::hidden(div(id="div_dm_stat_snp",tags$h4("eGFR association statistics separated by diabetes status", class="Output_header"),
													p("Shown are results from GWAS meta-analyses in European ancestry."),
													DT::dataTableOutput("dm_stat_snp"),
													tags$hr())),
								shinyjs::hidden(div(id="div_locus_zoom_snp",tags$h4("Locus Zoom Plot:", class="Output_header"),
													textOutput("LocusZoom_SNP_text"),
													uiOutput("plot_download_links_snp"),
												))
								
							))
				 ),
								tabPanel("Region", # region tab
										 useShinyjs(),
										 tags$h3("Region search",actionButton(inputId="question_region_search",label="?",class="question")),
										 wellPanel(
										  fluidRow(
											column(4, 
											  selectInput("Chromosome", label = "Chromosome", choices = setNames(as.list(1:22), paste("Chromosome", 1:22)), selected = 1)
											),
											column(8,
											  numericInput(inputId="pos_start", label="Start of region [bp]", value = 0),
											  numericInput(inputId="pos_end", label="End of region [bp]", value = 0),
											  shinyjs::hidden(div(id="region_go_div", p(actionButton(inputId="region_go", label="go", class="go"), class="go"))),
											),
										  )
										),

										 
										 
										 #verbatimTextOutput("value"),
										 shinyjs::hidden(
										   div(id="region_div",
											   span(textOutput("valid_gene_test")),
											   bsCollapse(id="regionCollapse",multiple=TRUE,
														  bsCollapsePanel("Overlapping genetic eGFRcrea signals",span(textOutput("ov_loci_test"),style="color:red"),
																		  DT::dataTableOutput("ov_loci")
														  ),
														  bsCollapsePanel("GPS entries for genes in overlapping eGFRcrea loci", 
																		  shinyjs::hidden(
																			div(id="ov_genes_down",
																				DT::dataTableOutput("GPS_region_search"),
																			)
																		  )
														  )
														  # ,
														  # bsCollapsePanel("Locus Zoom","hier mitunter", uiOutput("LocusZoom2"), textOutput("hier"))
											   ))),
								),
				 tabPanel("GPS", #GPS tab
						  
						  p(tags$h3("Gene Prioritisation - Overview",actionButton(inputId="question_GPS",label="?",class="question"))),
						  fluidRow( # all filter options are in this fluid row, spaces between the columns are a result of the css sheet
							style = "display: flex; flex-direction: column;",
							div(class = "gps-container",
								column(3,  id = "gpscontainersignal", 
									   h4("1. Signal-filter:"),
									   h4("Restrict to signals with any of the following properties regarding the strength of statistical support:"),
									   checkboxInput(inputId = "signalppafilter", label ="The signal's credible set contains a variant with a postior probability of association (PPA) of:", value = FALSE),
									   div(id="gpsppafilter", 
										   uiOutput("signalppafilterdetail")
										   ),

									   checkboxInput(inputId = "signalsmall", label ="signal has a small credible set (1-5 variants)", value = FALSE), # checkboxes don't need a selected value
									   div(id="nofiltersignal",checkboxInput(inputId = "signalnofilter", label =p("no filter (show all signals)"), value = TRUE) )
									   ,
									   tags$hr(),
									   
									   div(id="furthersignalfilter",h4("Restrict further to:"),
									   radioButtons(inputId = "signalcysbun", label="Signals located in a eGFRcys/BUN validated locus", inline = TRUE, choiceNames = c("yes", "no"), choiceValues = c(TRUE,FALSE), selected = FALSE),
									   
									   h4("Restrict further to:"),
									   radioButtons(inputId = "signalDM", label="Signals with differential association in individuals with diabetes vs. without diabetes", inline = TRUE, choiceNames = c("yes", "no"), choiceValues = c(TRUE,FALSE), selected = FALSE), #radio buttons always have a selected value
									   radioButtons(inputId = "signaldecline", label="Signals with association with eGFR decline", inline = TRUE, choiceNames = c("yes", "no"), choiceValues = c(TRUE,FALSE), selected = FALSE),
									   )

									   
								),
								column(5, id = "gpscontainermap", 
									   div(fluidRow( # opening a new fluid row inside the column of the parental fluid row.
										 column(8, 
												h4("2. Variant-to-gene mapping:"),
												h4("Restrict to genes which are mapped to a credible set variant by any of the following properties:"),
												div(class = "CADD",
												   checkboxGroupInput(inputId="genemapprot", 
																	  label=span("Gene contains a credible set variant that is protein-relevant"),  
																	  selected = NULL,
																	  inline = FALSE, width = NULL, 
																	  choiceNames = c("stop-gained, stop-lost, non-synonymus","canonical splice, noncoding change, synonymous, splice site","other functional consequence with high predicted deleteriousness (CADD-Phred \u2265 15)"), 
																	  choiceValues = c(11:13)
																	  ),
													
													),
												div(class = "eqtl",
													checkboxGroupInput(inputId="genemapeqtl", 
																	   label="Gene maps to a credible set variant that modulates gene expression (eQTLs) or splicing (sQTLs), FDR < 5%:",  
																	   selected = NULL,
																	   inline = FALSE, width = NULL, 
																	   choiceNames = c("eQTL in glomerular tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in tubulo-interstitial tissue (NEPTUNE, or Sheng et al [Nat.Genet. 2021])","eQTL in kidney cortex tissue (GTEx)","eQTL in other tissue (GTEx)","sQTL in kidney cortex tissue (GTEx)","sQTL in other tissue (GTEx)"), 
																	   choiceValues = c(14:19)
																	   ),
												
													)
												
												
										 ),
										 column(4, id="ppafiltergenemap", br(), uiOutput("detail.genemap.ppa"),
												
												)
									   )),
									   br(),
									   div(id="nofiltermap",checkboxInput(inputId = "mapnofilter", label =p("no filter (show all genes in selected signals)"), value = TRUE) )
									   
								),
								column(3, id = "gpscontainerfilter", 
									   h4("3. Gene-to-phenotype mapping:"), 
									   h4("Restrict to genes with any of the following properties:"),
									   div(class = "pheno",
										   checkboxGroupInput(inputId = "genelistpheno", 
															  label="Kidney phenotypes",
															  choiceNames = c("Gene has known kidney phenotypes in mouse models (Mouse Genome Informatics, MGI)","Gene is described to cause a human disease with kidney phenotype (online mendelian inheritance in man, OMIM, Groopman et al. [N.Engl.J.Med,2019], or Wopperer et al. [Kidney Int.,2022])"),
															  choiceValues = c("mouse","human"), selected=NULL),
										   ),
									   div(class = "drug",
										   checkboxGroupInput(inputId = "genelistdrug",
															  label = "Drug information",
															  choiceNames = c("Gene is a known drug target for kidney diseases (Therapeutic Target Database)","Gene is a known drug target for any other disease or with unspecific indication (Therapeutic Target Database)"),
															  choiceValues = c("kidney", "other"),
															  selected = NULL
											 
										   )),
									   div(id="nofiltergenelist",checkboxInput(inputId = "genelistnofilter", label =p("no filter (show all genes in selected signals)"), value = TRUE))
								)
							)
						  ),
						  br(),
						  actionButton(inputId="gpsgo", label="Go prioritize!"),
						  br(),
						  tags$hr(),
						  verbatimTextOutput("GPS_datadescription"),
						  
						  div(#class="overflow",
							  DT::dataTableOutput("GPS")),
						  br(),
						  div(downloadButton("downloadGPSTable", "Download Results"), p("Cave: filtering via column headers has no impact on the downloaded table", style="color: red;")), # here is an extra download button. What it does is defined in the server part
						  br(),
						  div(class="genesInGPS", textOutput("GPS_Genes1")),
						  br()
						  
						  
						  #tags$br(),
						  
						  
				 ),
							tabPanel("Documentation & Help", #Documentation Tab
									 navlistPanel( # like the navbarPage this allows for multiple Tabs but these are organized on the left side while their content is on the right
									 tabPanel("Release history",includeMarkdown("documentation_and_help/Release_history.Rmd")),
									 tabPanel("Gene Prioritisation (GPS) - User guide",includeMarkdown("documentation_and_help/Gene_prioritization.Rmd")),
									 tabPanel("Gene-, Variant-, and Region-Search - User Guide",includeMarkdown("documentation_and_help/Gene_search.Rmd")),
									 tabPanel("Data Sources", includeMarkdown("documentation_and_help/Data_sources.Rmd")),
									 tabPanel("Privacy, data security an License", includeMarkdown("documentation_and_help/Privacy.Rmd")),
									 tabPanel("Citation", includeMarkdown("documentation_and_help/Citation.Rmd")),
									 tabPanel("Contact",includeMarkdown("documentation_and_help/Contact.Rmd")),
									 widths = c(2,8) # left sidebar will ocupy 2 parts of the screen the right side will have 8 parts
									 ))
		)
	) # end of fluid page

