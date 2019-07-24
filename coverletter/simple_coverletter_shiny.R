

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}

movereports <- function(){
  moveRnwtexlog <- list.files(pattern="*.Rnw|*.tex|*.log")
  filesstrings::file.move(moveRnwtexlog, "pdfs/latexcode")
  
  movepdf <- list.files(pattern="*\\.pdf")
  filesstrings::file.move(movepdf, "pdfs")
}



# Coverletter Writing Function:
# -----------------------------

simple_coverletter <- function(
  yourname = "SRhoads, M.A.",
  youremail="srhoadsian@gmail.com",
  yourphone="(917)-408-3131",
  yourtitle="Data Scientist",
  yourtitle2="B.A., M.A (Columbia University)",
  company="SRhoadsian LLC",
  position="Cat Whisperer",
  paragraph1="",
  paragraph2="",
  paragraph3="",
  paragraph4="",
  filename_override=NULL,
  file="report.Rnw"
){
  
  top = c(
    "% !TeX program = XeLaTeX",
    "\\documentclass[]{formatting}",
    "\\usepackage{booktabs}",
    "\\usepackage{makecell}",
    "\\usepackage{colortbl}",
    "\\usepackage{xcolor}",
    "\\usepackage{caption}",
    "\\usepackage{adjustbox}",
    "\\usepackage{arydshln}",
    "\\usepackage{array, multirow, graphicx}",
    "\\usepackage{makecell}",
    "\\usepackage{enumitem}",
    
    "\\renewcommand{\\topfraction}{.85}",
    "\\renewcommand{\\bottomfraction}{.7}",
    "\\renewcommand{\\textfraction}{.15}",
    "\\renewcommand{\\floatpagefraction}{.66}",
    "\\renewcommand{\\dbltopfraction}{.66}",
    "\\renewcommand{\\dblfloatpagefraction}{.66}",
    "\\setcounter{topnumber}{9}",
    "\\setcounter{bottomnumber}{9}",
    "\\setcounter{totalnumber}{20}",
    "\\setcounter{dbltopnumber}{9}",
    "",
    "\\usepackage[referable]{threeparttablex}",
    "\\renewlist{tablenotes}{enumerate}{1}",
    "\\makeatletter",
    "\\setlist[tablenotes]{label=\\tnote{\\alph*},ref=\\alph*,itemsep=\\z@,topsep=\\z@skip,partopsep=\\z@skip,parsep=\\z@,itemindent=\\z@,labelindent=\\tabcolsep,labelsep=.1em,leftmargin=*,align=left,before={\\tiny}}",
    "\\makeatother",
    "",
    "\\usepackage[printwatermark]{xwatermark}",
    "\\usepackage{tikz}",
    "\\captionsetup{aboveskip=2pt}",
    "",
    "\\sethyphenation[variant=british]{english}{}",
    "\\definecolor{MyRed}{rgb}{0.8901961 0.2901961 0.2000000}",
    "\\definecolor{mygray}{rgb}{0.9411765 0.9411765 0.9411765}",
    "\\colorlet{tablerowcolor}{MyRed!10}",
    "\\newcommand{\\rowcol}{\\rowcolor{tablerowcolor}}",
    "\\usepackage{helvet}",
    "\\begin{document}",
    ""
  ) %>% 
    paste0(collapse = '\n')
  
  bottom = c(
    "",
    "\\end{document}"
  ) %>% 
    paste0(collapse = '\n')
  
  midbit <- c(
    
    "\\colorbox{mygray}{",
    "\\parbox[t]{\\linewidth}{",
    "\\vspace*{14pt} ",
    "\\hfill \\color{white} \\textsc{\\huge ", yourname, " }",
    "\\vspace*{14pt} ",
    "}}",
    
    "\\large{\\\\ \\\\",
    as.character(Sys.Date()), "\\hfill ", "\\textbf{", youremail, "} \\\\",
    "}",
    
    "\\large{",
    "\\vspace*{14pt}",
    "\\hfill \\color{white} {\\textbf{\\large", yourphone, "}}",
    "}",
    
    "\\\\",
    
    "\\large{Dear \\textbf{", company, "},}",
    
    "\\\\",
    "\\\\",
    
    "\\large{",
    "I am writing to express my interest in your company's \\textbf{", position,"} position.", paragraph1, " 
    
    Working as a ", position, " at", company, " would allow me to apply the skills and training that I have developed throughout my career. ", paragraph2, "
    
    ", paragraph3, "
    
    ", paragraph4, "
    
    The chance to work as a ", position, " at", company, " is an intriguing one. From what I’ve researched about your firm, the work that I would be involved in is exactly what I have been seeking. I'm looking forward to corresponding further!",
    "}",
    
    "\\\\",
    "\\\\",
    
    # "djf",
    "\\large{Sincerely,}\\\\",
    "\\\\",
    "\\large{\\textbf{", yourname, "}}\\\\",
    "\\\\",
    "\\small{\\textbf{", yourtitle, "}}\\\\",
    "\\small{\\textbf{", yourtitle2, "}}\\\\"
  ) %>% 
    paste0(collapse = '\n')
  
  latextext <- paste0(top, midbit, bottom,  collapse="\n")
  
  filename_rnw <- "report.Rnw"#paste0("coverletter_", company, "_", Sys.Date(), ".Rnw")
  filename_tex <- "report.tex"#paste0("coverletter_", company, "_", Sys.Date(), ".tex")
  
  # if(!is.null(filename_override)){
  #   filename_rnw <- paste0(filename_override, ".Rnw")
  #   filename_tex <- paste0(filename_override, ".tex")
  # }
  
  write(latextext, file = 'report.Rnw')
  knit2pdf(input = 'report.Rnw', output = 'report.tex', compiler = 'xelatex', envir = environment())
  
  # movereports() # SRHOADS added
  
  print("LOL Congrats. All done")
}

# print("...Seriously, if u forget those, then you'll tell all the companies u wanna work for SRhoads as a cat whisperer")

# jadrian_coverletter()





