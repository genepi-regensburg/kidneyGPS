


my.debugger=function(a_input, a_show){

	if (a_show==FALSE)
	return (-1)


	print("The kidney open chromatin eQTL checkbox was selected!")
	print("These are the elements of INPUT:")	
	print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
	print("")
	print("")
	print("")
	print("")
	print("Names of INPUT:")
	print("")
		print(names(a_input))

	print("")
	print("")
	print("COLUMNS1 of INPUT:")
	print("")		
		print(a_input$columns1)
	print("")
	print("")
	print("COLUMNS2 of INPUT:")
	print("")
		print(a_input$columns2)

	print("")
	print("")
	print("COLUMNS3 of INPUT:")
	print("")
		print(a_input$columns3)
	print("")
	print("")
	print("COLUMNS4 of INPUT:")
	print("")
		print(a_input$columns4)
	print("")
	print("")
	print("detail_evidence_locus_gene of INPUT:")
		print(a_input$detail_evidence_locus_gene)	
	print("")
	print("")
	print("gene.locuszoom of INPUT:")
		print(a_input$gene.locuszoom)	
	
	
	
	
}


# Define the debug function
fct.debug <- function(label, value = NULL) {
  cat("DEBUG -", label, ":\n")
  print(value)
  cat("------\n")
}
