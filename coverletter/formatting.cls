% -- Encoding UTF-8 without BOM

\ProvidesClass{formatting}[2015/02/27 CV class]
\NeedsTeXFormat{LaTeX2e}

\DeclareOption{espanol}{\def\@cv@espanol{}}
\DeclareOption{print}{\def\@cv@print{}}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass{article}

%----------------------------------------------------------------------------------------
% Colors %
%----------------------------------------------------------------------------------------

\RequirePackage{xcolor}

\definecolor{white}{RGB}{255,255,255}
\definecolor{skyblue}{HTML}{b6cee2}
\definecolor{darkgray}{HTML}{333333}
\definecolor{gray}{HTML}{4D4D4D}
\definecolor{lightgray}{HTML}{999999}

\definecolor{blue}{HTML}{2b506e}
\definecolor{red}{HTML}{FA2772}
\definecolor{orange}{HTML}{FE9720}
\definecolor{green}{HTML}{A7E22E}
\definecolor{purple}{HTML}{9358FE}
\definecolor{aquamarine}{HTML}{36AF90}

\definecolor{date}{HTML}{A4A4A4}

\ifdefined\@cv@print
  \colorlet{blue}{gray}
  \colorlet{red}{gray}
  \colorlet{orange}{gray}
  \colorlet{green}{gray}
  \colorlet{purple}{gray}
  \colorlet{aquamarine}{gray}
  \colorlet{fillheader}{white}
  \colorlet{header}{gray}
\else
  \colorlet{fillheader}{blue}
  \colorlet{header}{white}
\fi
\colorlet{textcolor}{gray}
\colorlet{headercolor}{gray}

%----------------------------------------------------------------------------------------
% Fonts %
%----------------------------------------------------------------------------------------

\RequirePackage[quiet]{fontspec}
% \RequirePackage[math-style=TeX,vargreek-shape=unicode]{unicode-math}

\newfontfamily\bodyfont[Path=fonts/]{Roboto-Regular}%[Path=./]%[Path=fonts/]
\newfontfamily\bodyfontit[Path=fonts/]{Roboto-LightItalic}%[Path=./]%[Path=fonts/]
\newfontfamily\thinfont[Path=fonts/]{Roboto-Thin}%[Path=./]%[Path=fonts/]
\newfontfamily\headingfont[Path=fonts/]{RobotoCondensed-Bold}%[Path=./]%[Path=fonts/]

\defaultfontfeatures{Mapping=tex-text}
\setmainfont[Mapping=tex-text, Color=textcolor, Path = fonts/]{Roboto-Light} %Path = fonts/]{Roboto-Light}

\newcommand{\italica}[1]{%
    {\color{gray}\bodyfontit #1}%
}

%----------------------------------------------------------------------------------------
% Header %
%----------------------------------------------------------------------------------------

\RequirePackage{tikz}

\newcommand{\header}[2]{%
  \begin{tikzpicture}[remember picture,overlay]
    \node [rectangle, fill=fillheader, anchor=north, minimum width=\paperwidth, minimum height=2.5cm] (box) at (current page.north){};
    \node [anchor=center] (name) at (box) {%
      \fontsize{28pt}{45pt}\color{header}%
      {\bodyfont #1}{\thinfont  #2}
    };
  \end{tikzpicture}
  \vspace{1cm}
  \vspace{-2\parskip}
}
%----------------------------------------------------------------------------------------
% footer %
%----------------------------------------------------------------------------------------

\RequirePackage{tikz}

\newcommand{\footer}[2]{%
  \begin{tikzpicture}[remember picture,overlay]
    \node [rectangle, fill=fillheader, anchor=south, minimum width=\paperwidth, minimum height=0.5cm] (box) at (current page.south){};
    \node [anchor=center] (name) at (box) {%
      \fontsize{8pt}{16.75pt}\color{skyblue}%
      {\bodyfont #1}{\thinfont  #2}
    };

  \end{tikzpicture}
  \vspace{1cm}
  \vspace{-2\parskip}
}

%----------------------------------------------------------------------------------------
%	Last updated command %
%----------------------------------------------------------------------------------------

\RequirePackage[absolute,overlay]{textpos}
\RequirePackage{polyglossia}

% \RequirePackage[en-US]{datetime2}

\newcommand{\sethyphenation}[3][]{%
  \sbox0{\begin{otherlanguage}[#1]{#2}
    \hyphenation{#3}\end{otherlanguage}}}
%\sethyphenation[<options>]{<language>}{<list of words separated by spaces>}

\ifdefined\@cv@espanol
  \setdefaultlanguage{spanish}
  \def\lastupdatedtext{Última Actualización el}
\else
  \setdefaultlanguage{english}
  \def\lastupdatedtext{Report run on}
\fi

\setlength{\TPHorizModule}{0.01\paperwidth}
\setlength{\TPVertModule}{0.01\paperwidth}

\newcommand{\lastupdated}{ 
  \begin{textblock}{10}(1.3, 0.05)
    \raggedright
    \fontsize{8pt}{10pt}\color{skyblue}\thinfont 
    \lastupdatedtext{} \today
  \end{textblock}}

%----------------------------------------------------------------------------------------
% Structure %
%----------------------------------------------------------------------------------------
\RequirePackage{parskip}

\newcounter{colorCounter}
\def\@sectioncolor#1#2#3{%
  {%
    \color{%
      \ifcase\value{colorCounter}%
        headercolor\or%
        headercolor\or%
        headercolor\or%
        headercolor\or%
        headercolor\or%
        headercolor\else%
        headercolor\fi%
    } #1#2#3%
  }%
  \stepcounter{colorCounter}%
}

\renewcommand{\section}[1]{
  {\par\vspace{\parskip}
  {%
    \LARGE\headingfont\color{headercolor}%
    \@sectioncolor #1%
  }
  \par\vspace{\parskip}}
}

\renewcommand{\subsection}[2]{
  \par\vspace{.5\parskip}%
  \Large\headingfont\color{headercolor} #2%
  \par\vspace{.25\parskip}%
}

\newcommand{\jobtitle}[1]{%
    {\color{gray}\bodyfontit #1}%
}

\pagestyle{empty}

%----------------------------------------------------------------------------------------
% List environment %
%----------------------------------------------------------------------------------------

\setlength{\tabcolsep}{0pt}
\newenvironment{entrylist}{%
  \begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}ll}
}{%
  \end{tabular*}
}
\renewcommand{\bfseries}{\headingfont\color{headercolor}}
\newcommand{\entry}[4]{%
  #1&\parbox[t]{\paperwidth}{%
    \textbf{#2}%
    \hfill%
    {\footnotesize\addfontfeature{Color=lightgray} #3}\\%
    #4\vspace{\parsep}%
  }\\}

%----------------------------------------------------------------------------------------
% Other tweaks %
%----------------------------------------------------------------------------------------

\RequirePackage[letterpaper, left=1cm,top=1cm,right=1cm,bottom=1cm,nohead,nofoot]{geometry}
\RequirePackage{hyperref}
\hypersetup{
    pdftitle=Workforce \textbar{} Analytics,
    pdfauthor=Jason O'Rawe,
    pdfsubject=Workforce Analytics,%
    }