\LoadClass[11pt]{article}
\usepackage{calc, comment, eurosym, fancyhdr, lastpage, color, hyperref}
\makeatletter
\newlength{\bibhang}
\setlength{\bibhang}{1em}
\newlength{\bibsep}{\@listi \global\bibsep\itemsep \global\advance\bibsep by\parsep}
\newenvironment{bibsection}{\begin{enumerate}{}{\setlength{\leftmargin}{\bibhang}\setlength{\iteminden}{-\leftmargin}\setlength{\itemsep}{\bibsep}\setlength{\parsep}{\z@}\setlength{\partopsep}{0pt}       \setlength{\topsep}{0pt}}}{\end{enumerate}\vspace{-.6\baselineskip}}
\makeatother
\reversemarginpar
\usepackage[paper=letterpaper, marginparwidth=1.2in, marginparsep=.05in, margin=1in, includemp]{geometry}
\setlength{\parindent}{0in}
\usepackage[shortlabels]{enumitem}
\pagestyle{fancy}
\fancyhf{}\renewcommand{\headrulewidth}{0pt}
\fancyfootoffset{\marginparsep+\marginparwidth}
\newlength{\footpageshift}
\setlength{\footpageshift}
          {0.5\textwidth+0.5\marginparsep+0.5\marginparwidth-2in}
\lfoot{\hspace{\footpageshift}
       \parbox{4in}{\, \hfill
                    \arabic{page} of \protect\pageref*{LastPage} 
                    \hfill \,}}
\definecolor{darkblue}{rgb}{0.0,0.0,0.5}
\hypersetup{colorlinks, breaklinks, linkcolor=darkblue, urlcolor=darkblue, anchorcolor=darkblue, citecolor=darkblue, pdftoolbar=true, pdfmenubar=true, pdffitwindow=false, pdfstartview={FitH}}

\newcommand{\makeheading}[2][]
        {\hspace*{-\marginparsep minus \marginparwidth}
         \begin{minipage}[t]{\textwidth+\marginparwidth+\marginparsep}
             {\large \bfseries #2 \hfill #1}\\[-0.15\baselineskip]
                 \rule{\columnwidth}{1pt}
         \end{minipage}}

\renewcommand{\section}[1]{\pagebreak[3]
    \hyphenpenalty=10000
    \vspace{1.3\baselineskip}
    \phantomsection\addcontentsline{toc}{section}{#1}
    \noindent\llap{\scshape\smash{\parbox[t]{\marginparwidth}{\raggedright #1}}}
    \vspace{-\baselineskip}\par}

\newenvironment{outerlist}[1][\enskip\textbullet]
        {\begin{itemize}[#1,leftmargin=*]}{\end{itemize}
         \vspace{-.6\baselineskip}}

\newenvironment{lonelist}[1][\enskip\textbullet]
        {\begin{list}{#1}{
        \setlength{\partopsep}{0pt}
        \setlength{\topsep}{0pt}}}
        {\end{list}\vspace{-.6\baselineskip}}
    
\newenvironment{innerlist}[1][\enskip\textbullet]
        {\begin{itemize}[#1,leftmargin=*,parsep=0pt,itemsep=0pt,topsep=0pt,partopsep=0pt]}
        {\end{itemize}}

\newenvironment{loneinnerlist}[1][\enskip\textbullet]%
        {\begin{itemize}[#1,leftmargin=*,parsep=0pt,itemsep=0pt,topsep=0pt,partopsep=0pt]}
        {\end{itemize}\vspace{-.6\baselineskip}}

\newcommand{\blankline}{\quad\pagebreak[3]}
\newcommand{\halfblankline}{\quad\vspace{-0.5\baselineskip}\pagebreak[3]}

\newcommand\doilink[1]{\href{http://dx.doi.org/#1}{#1}}
\newcommand\doi[1]{doi:\doilink{#1}}

\providecommand*\url[1]{\href{#1}{#1}}
\renewcommand*\url[1]{\href{#1}{\texttt{#1}}}
\providecommand*\email[1]{\href{mailto:#1}{#1}}
\providecommand\BibTeX{{B\kern-.05em{\sc i\kern-.025em b}\kern-.08em\TeX}}
\providecommand\Matlab{\textsc{Matlab}}
