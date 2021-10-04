%% ----------------------------------------------------------------------------
%% This LaTeX 2e Class file is based on the uathesis class made for 
%% the University of Arizona and adapted for Rutgers University.
%%
%% For specific formatting details see: 
%% https://grad.rutgers.edu/academics/graduation/electronic-thesis-and-dissertation-style-guide
%%
%% Acknowledgements for previous contributors: 
%% 
%% Peter Halverson    1989 (non-LPL)
%% William D. Sears   1994
%% Rov Vervack        1996
%% Andrew Rivkin      1997
%% Joe Spitale        2001
%% Dave O'Brien       2003
%% Ross A. Beyer      2004
%% Jim Richardson     2005
%% Terry Hurford      2005
%% Curtis S. Cooper   2007
%% David A. Minton    2009
%% Joseph V. Casillas 2015
%% Joseph V. Casillas 2021
%%
%% ----------------------------------------------------------------------------


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ruthesis}[2021/08/16 Rutgers University Thesis Class]

\RequirePackage{ifthen}
\RequirePackage{cclicenses} % Generate the Creative Commons icons. 


%% This class file is based off of the book class.
%% We still need to set up some options that are specific to the various kinds 
%% of theses.

% Set a default for ThesisType, just in case
\newcommand{\ThesisType}{\emph{(please specify thesis, dissertation, or document as an option in the ruthesis documentclass statement at the top of your file)}}

\DeclareOption{thesis}{%
  \renewcommand{\ThesisType}{Thesis}
}

\DeclareOption{dissertation}{%
  \renewcommand{\ThesisType}{Dissertation}
}

\DeclareOption{document}{%
  \renewcommand{\ThesisType}{Document}
}

\newboolean{Copyright}
\setboolean{Copyright}{false}
\DeclareOption{copyright}{%
  \setboolean{Copyright}{true}
}

% Code for including Creative Commons licenses
\newboolean{CCBY}
\setboolean{CCBY}{false}
\newboolean{CCBYSA}
\setboolean{CCBYSA}{false}
\newboolean{CCBYND}
\setboolean{CCBYND}{false}

\DeclareOption{CC-BY}{%
  \setboolean{CCBY}{true}
}
\DeclareOption{CC-BY-SA}{%
  \setboolean{CCBYSA}{true}
}
\DeclareOption{CC-BY-ND}{%
  \setboolean{CCBYND}{true}
}

\newboolean{GeneratedOn}
\setboolean{GeneratedOn}{false}
\DeclareOption{generatedon}{%
  \setboolean{GeneratedOn}{true}
} % This option puts the generation date on the titlepage.


%% These next four options are for passing through to the book class.
\DeclareOption{oneside}{%
  \PassOptionsToClass{\CurrentOption}{book}%
}

\DeclareOption{oneside}{%
  \PassOptionsToClass{\CurrentOption}{book}%
} 

\DeclareOption{oneside}{%
  \PassOptionsToClass{\CurrentOption}{book}%
}

\DeclareOption{oneside}{%
  \PassOptionsToClass{\CurrentOption}{book}%
}

\DeclareOption*{%
  \ClassWarning{ruthesis}{Unknown option `\CurrentOption'}%
} % A general warning for unknown options.


\ExecuteOptions{oneside,openright} % this provides a default option list

\ProcessOptions\relax
\LoadClass[12pt]{book}


%% Start changes from the book class.
% We use \sloppy to suppress word division and permit larger interword
% spacing so that lines are broken between words.
\sloppy


%% Global Settings ------------------------------------------------------------

% Set document geometry
\usepackage[top=1in, bottom=1in, left=1.5in, right=1in, includefoot]{geometry}

% Page Style to put numbers at the bottom of front matter documents
\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhead[L]{}
\fancyhead[R]{}
\cfoot{\thepage}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\setlength{\headheight}{14.49998pt} % removes warning


%% Double spaced 
%% (in order to conserve paper, we'll actually make this less than 2)
\renewcommand{\baselinestretch}{1.4}


%% Set margins for the bulk of dissertation 
%% (referred to as Body of Paper).

%\setlength{\topmargin}{0in}     % = 1in because LaTeX adds 1in
%\setlength{\headheight}{0in}   % Height of page numbers
\setlength{\headsep}{0.75in}     % Distance from top of pagenum to text.

%\addtolength{\headsep}{-\headheight} % Adj. for height of pagenumber.
%\setlength{\topskip}{12pt}      % This is the height of the text.
%\setlength{\footskip}{0in}      % ???
%\setlength{\oddsidemargin}{0.5in} % = 1.5in because LaTeX adds 1in
%\setlength{\evensidemargin}{0in}  % = 1in because LaTeX adds 1in
\setlength{\textheight}{8.4in}  % should be 8.5in, see below
%\setlength{\textwidth}{5.9in}   % should be 6in but we're being 
                                % conservative, just to be safe.
\setlength{\skip\footins}{2pc plus 5pt} % add space above footnotes
\setlength{\footnotesep}{1.5pc} % space between footnotes


% Make sure that various key names are not in caps
\renewcommand{\partname}{PART}
\renewcommand{\chaptername}{Chapter}
\renewcommand{\appendixname}{Appendix}
\renewcommand{\contentsname}{TABLE OF CONTENTS}
\renewcommand{\listfigurename}{LIST OF FIGURES}
\renewcommand{\listtablename}{LIST OF TABLES}
\renewcommand{\bibname}{References}




%% General Renewed Commands ---------------------------------------------------
%% 
%% There are several key and subtle LaTeX commands that need to be
%% altered in minor ways from the way they are defined in book.cls.
%% This section 'renew's or redefines them for our use.  
%% In all other sections of this class file, we have strived to use the 
%% abstract generic LaTeX commands rather than the more direct (and 
%% potentially dangerous) TeX commands. 
%% Since this section renews commands taken from the LaTeX book class 
%% file, we follow the (less strict) conventions in that file, so that 
%% we are making the minimal changes to these commands to make them 
%% work for us.

% We are renewing the following two commands so that there are
% appropriate dot leaders between the TOC item and that TOC item's
% page number.
% We replaced the \hfil line with a sequence of commands taken from the 
% latex.ltx file for \@dottedtocline (instead of just using \dotfill 
% which gave dots with a slightly different spacing).
% Similarly, we left the page numbers bold, rather than making them
% normalfont.
% 
% The lines that were altered are still present below, but commented out.
% Hopefully this will make it easier to integrate with future versions
% of these commands in LaTeX.
%
% It appears that the formatters no longer want to allow any boldfacing
% in TOC-related items, so \bfseries items have been commented out 
% below.
%
% Otherwise these two commands are identical to those in the 
% 2001/04/21 v1.4e book.cls.
%
\renewcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \setlength\@tempdima{3em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       %\large \bfseries %#1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
	   {#1}\nobreak \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{.}\mkern \@dotsep mu$}\hfill\nobreak
       %\hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #2}%
       \hb@xt@\@pnumwidth{\hfil \normalcolor #2}%
       \par}
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
    \endgroup
  \fi}

\renewcommand*\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries     % <- this is make chapters bold
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      %#1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
      {#1}\nobreak \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{.}\mkern \@dotsep mu$}\hfill\nobreak
      %\hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #2}%
      \hb@xt@\@pnumwidth{\hfil \normalcolor #2}%
      \par
      \penalty\@highpenalty
    \endgroup
  \fi}

% We renewed the following command so that the chapter level items in
% the TOC have the correct word (nothing|Chapter|Appendix) in front of
% them.
%
% Otherwise this command is identical to \def\@chapter in the 
% 2001/04/21 v1.4e book.cls.
\renewcommand\@chapter[2][\#2]{\ifnum \c@secnumdepth >\m@ne
  \if@mainmatter
    \refstepcounter{chapter}%
    \typeout{\@chapapp\space\thechapter.}%
    \addcontentsline{toc}{chapter}%
      %{\protect\numberline{\thechapter}#1}%
      {\@chapapp\space\protect\numberline{\thechapter:}#1}%
  \else
    \addcontentsline{toc}{chapter}{#1}%
  \fi
  \else
    \addcontentsline{toc}{chapter}{#1}%
  \fi
  \chaptermark{#1}%
  \addtocontents{lof}{\protect\addvspace{10\p@}}%
  \addtocontents{lot}{\protect\addvspace{10\p@}}%
  \if@twocolumn
    \@topnewpage[\@makechapterhead{#2}]%
  \else
    \@makechapterhead{#2}%
    \@afterheading
  \fi}

% In the same vein as above, the following was renewed so that part
% level items in the TOC will have the correct word. 
% Similarly, font size and bolding are removed to stay consistent 
% with TOC.
%
% Otherwise this command is identical to \def\@part in the 
% 2004/02/16 v1.4f book.cls
\renewcommand\@part[2][\#2]{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}%{\thepart\hspace{1em}#1}%
    {\partname\space\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
       %\huge\bfseries \partname\nobreakspace\thepart
       \partname\nobreakspace\thepart
       \par
       \vskip 20\p@
     \fi
     %\Huge \bfseries #2\par}%
     #2\par}%
    \@endpart}


% We renewed the following commands so that when the chapter headings are
% printed out on the first page of the chapter, they are the same font
% size as in the TOC. 
% Otherwise this command is identical to \def\@makechapterhead and
% \def\@makeschapterhead in the 2001/04/21 v1.4e book.cls.
\renewcommand{\@makechapterhead}[1]{%
  %\vspace*{50\p@}%
  {\parindent \z@ \raggedright \normalfont
    \begin{center}
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
        \bfseries \@chapapp\space \thechapter:
      \fi
    \fi
    \interlinepenalty\@M
    \bfseries #1\par\nobreak
  \end{center}
  }}

\renewcommand{\@makeschapterhead}[1]{%
  %\vspace*{50\p@}%
  {\parindent \z@ \raggedright
  	\begin{center}
    \normalfont
    \interlinepenalty\@M
    %\Huge \bfseries  #1\par\nobreak
    %\bfseries  #1\par\nobreak
    #1\par\nobreak
	\end{center}
    \vskip 40\p@
  }}

% We renewed the chapter command simply so that we could comment out the
% pagestyle and put in the empty \markboth. 
% We want the page numbers (even on the first page of a chapter) to be 
% in the header. Similarly, in the
% TOC, LOT, and LOF (the only sections with page headers) we don't want
% headers on the first page.
%
% Otherwise this command is identical to \newcommand\chapter
% in the 2004/02/16 v1.4f book.cls

\renewcommand{\chapter}{
  \if@openright\cleardoublepage\else\clearpage\fi
    \thispagestyle{plain}%
    \global\@topnum\z@
    \@afterindentfalse
    \markboth{}{}        % <--added
    \secdef\@chapter\@schapter}

% Same as above, but for the part command, otherwise identical to 
% \newcommand\part in the 2004/02/16 v1.4f book.cls
\renewcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  %\thispagestyle{plain}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \markboth{}{} % <--added
  \null\vfil
  \secdef\@part\@spart}

% The following two commands were renewed so that these items don't 
% have a larger font size than everything else. 
% Not required, but it makes things fit in since the chapter title 
% can't be big.
%
% 
% Otherwise this command is identical to the \section and \subsection
% newcommands in the 2001/04/21 v1.4e book.cls.
\renewcommand{\section}{\@startsection {section}{1}{\z@}%
							{-3.5ex \@plus -1ex \@minus -.2ex}%
							{2.3ex \@plus.2ex}%
							{\normalsize\bfseries}} %<-- makes section bold
							%{\normalsize}}
							%{\normalfont\Large\bfseries}}

\renewcommand{\subsection}{\@startsection{subsection}{2}{\z@}%
							{-3.25ex\@plus -1ex \@minus -.2ex}%
							{1.5ex \@plus .2ex}%
							{\normalsize\bfseries}} %<-- makes subsection bold
							%{\normalsize}}
							%{\normalfont\large\bfseries}}

\renewcommand{\subsubsection}{\@startsection{subsubsection}{2}{\z@}%
              {-3.25ex\@plus -1ex \@minus -.2ex}%
              {1.5ex \@plus .2ex}%
              {\normalsize\bfseries}} %<-- makes subsubsection bold
              %{\normalsize}}
              %{\normalfont\large\bfseries}}

\renewcommand{\paragraph}{\@startsection{paragraph}{2}{\z@}%
              {-3.25ex\@plus -1ex \@minus -.2ex}%
              {1.5ex \@plus .2ex}%
              %{\normalsize\bfseries}} %<-- makes paragraph bold
              {\normalsize}}
              %{\normalfont\large\bfseries}}

% The following four commands were renewed so that text in the body
% and captions of figures and tables will be single spaced and not
% get the document's default spacing.
%
% Otherwise this command is identical to the \figure and \table
% newenvironments in the 2001/04/21 v1.4e book.cls.

\renewenvironment{figure}
  {
  \renewcommand{\baselinestretch}{1} % single spaced here
  \small\normalsize % size switch to make the spacing behave
  \@float{figure}}
  {\end@float}


\renewenvironment{figure*}
  {
  \renewcommand{\baselinestretch}{1} % single spaced here
  \small\normalsize % size switch to make the spacing behave
  \@dblfloat{figure}}
  {\end@dblfloat}

\renewenvironment{table}
  {
  \renewcommand{\baselinestretch}{1} % single spaced here
  \small\normalsize % size switch to make the spacing behave
  \@float{table}}
  {\end@float}

\renewenvironment{table*}
  {
  \renewcommand{\baselinestretch}{1} % single spaced here
  \small\normalsize % size switch to make the spacing behave
  \@dblfloat{table}}
  {\end@dblfloat}


%% These commands give us values for strings that we will use 
%% more than once in the commands below.

\newcommand{\completetitle}[1]{%
\newcommand{\CompleteTitle}{#1}
}

\newcommand{\fullname}[1]{%
\newcommand{\FullName}{#1}
}

\newcommand{\degreename}[1]{%
\newcommand{\DegreeName}{#1}
}

\newcommand{\programname}[1]{%
\newcommand{\ProgramName}{#1}
}

\newcommand{\directorname}[1]{%
\newcommand{\DirectorName}{#1}
}

\newcommand{\defensemonth}[1]{%
\newcommand{\DefenseMonth}{#1}
}

\newcommand{\defenseyear}[1]{%
\newcommand{\DefenseYear}{#1}
}




%
% Start front matter type sections
%

%% Copyright Page -------------------------------------------------------------

\newcommand{\makecopyright}{%

{
\thispagestyle{empty} % no page number on title page
\renewcommand{\baselinestretch}{1} % single spaced here
\small\normalsize % size switch to make the spacing behave

\begin{center}
\vspace*{2in}
\copyright \DefenseYear \\
\medskip
\FullName \\
\medskip
ALL RIGHTS RESERVED \\
\end{center}
\clearpage
}
}

%% ----------------------------------------------------------------------------







%% Title Page -----------------------------------------------------------------
%%
%% options are: 
%%    #1 = Title
%%    #2 = Author
%%    #3 = degree title
%%    #4 = director name
%%    #5 = defense month
%%    #6 = defense year
%%
%% In the template these are set in `doc_prefix.tex`

\newcommand{\maketitlepage}{%

{
\thispagestyle{empty} % no page number on title page
\renewcommand{\baselinestretch}{2}
\small\normalsize % make the spacing behave
\vspace*{-1.0in} % Too much space at the top

\begin{center}
{\MakeUppercase{\CompleteTitle}} \\
By \\
{\MakeUppercase{\FullName}} \\
A \ThesisType\ submitted to the \\
School of Graduate Studies \\
Rutgers, the State University of New Jersey \\
In partial fulfillment of the requirements \\
For the degree of \\
\DegreeName \\
Graduate Program in \ProgramName \\
Written under the direction of \\
\DirectorName \\
And approved by \\
\bigskip
\rule{3in}{0.75pt}\\
\rule{3in}{0.75pt}\\
\rule{3in}{0.75pt}\\
\rule{3in}{0.75pt}\\
New Brunswick, New Jersey \\
\DefenseMonth\ \DefenseYear \\
\end{center}

}
}

%% ----------------------------------------------------------------------------










%% Abstract -------------------------------------------------------------------
%%
%% This command includes the file that contains the abstract.
%%
%% \incabstract
%% Option is #1 = abstract file name
%%

\newcommand{\incabstract}[1]{

{
\renewcommand{\baselinestretch}{2}
\small\normalsize % make the spacing behave

\pagenumbering{roman}

\chapter*{ABSTRACT OF THE DISSERTATION}
\addcontentsline{toc}{chapter}{ABSTRACT}

\begin{center}
\vspace*{-0.5in}
\CompleteTitle \\
by \\
{\MakeUppercase{\FullName}} \\
\bigskip
\bigskip
Dissertation Director: \\
\DirectorName \\
\vspace*{1in}
\end{center}

\InputIfFileExists{#1}
	{\typeout{Including your file #1.tex as the abstract.}}
	{Couldn't find #1.tex to include as your abstract.}

}
}

%% ----------------------------------------------------------------------------








%% Acknowledgements -----------------------------------------------------------
%%
%% This command includes the file that contains the acknowledgements.
%% 
%% \incacknowledgements
%% Option is #1 = acknowledgements file name
%%

\newcommand{\incacknowledgements}[1]{

% This bracket begins an unnmaed environment in which we will temporarily
% override the document's page style.

{
\renewcommand{\baselinestretch}{2} % Update (20211004): Requires double space
\small\normalsize % size switch to make the spacing behave

\chapter*{ACKNOWLEDGEMENTS}
\addcontentsline{toc}{chapter}{ACKNOWLEDGEMENTS}

\InputIfFileExists{#1}
	{\typeout{Including your file #1.tex as the acknowledgements.}}
	{Couldn't find #1.tex to include as your acknowledgements.}

}
}

%% ----------------------------------------------------------------------------









%% Dedication -----------------------------------------------------------------
%%
%% This command includes the file that contains the dedication.
%% 
%% \incdedication
%% Option is #1 = acknowledgements file name
%%

\newcommand{\incdedication}[1]{

{
\renewcommand{\baselinestretch}{1} % single spaced here
\small\normalsize % size switch to make the spacing behave

\chapter*{DEDICATION}
\addcontentsline{toc}{chapter}{DEDICATION}

\InputIfFileExists{#1}
	{\typeout{Including your file #1.tex as the acknowledgements.}}
	{Couldn't find #1.tex to include as your acknowledgements.}

}
}

%% ----------------------------------------------------------------------------










%% Table of Contents ----------------------------------------------------------
%% 
%% The table of contents that the UofA wants is subtley different in a
%% number of ways such that we need to redefine the \tableofcontents
%% command provided by the basic LaTeX classes.  Many of the commands
%% here are confusing, but were taken from the LaTeX book.cls class
%% file with the following date and version stamp: 2001/04/21 v1.4e
%%
\renewcommand{\tableofcontents}{

%\addtolength{\headsep}{0in}		% These adjust things for the running
%\addtolength{\textheight}{-1in}	% headers for TOC, LOF, LOT.

% This bracket begins an unnmaed environment in which we will temporarily
% override the document's page style and margins
{
\renewcommand{\baselinestretch}{1}	% single spaced here
\small\normalsize					% size switch to make the spacing behave

\if@twocolumn					% \
	\@restonecoltrue\onecolumn	%  \
\else							% 	> Taken from book.cls
	\@restonecolfalse			%  /
\fi								% /

% vspace added to account for the TOC, LOF, & LOT running header business.
\chapter*{\vspace*{-0.5in}\contentsname}
\addcontentsline{toc}{chapter}{TABLE OF CONTENTS}
\vspace*{-0.5in}
% \markboth{	\protect\raisebox{-0.5in}{\textbf{\emph{\contentsname} -- Continued}\hspace{1.5in}} }{
% 			\protect\raisebox{-0.5in}{\hspace{1.5in}\textbf{\emph{\contentsname} -- Continued}} }
\markboth{	\protect\raisebox{-0.5in}{{\emph{\contentsname} -- Continued}\hspace{1.5in}} }{
			\protect\raisebox{-0.5in}{\hspace{1.5in}{\emph{\contentsname} -- Continued}} }

\@starttoc{toc}%
\if@restonecol\twocolumn\fi % Taken from book.cls

}
}

%% ----------------------------------------------------------------------------









%% List of Tables -------------------------------------------------------------
%% 
%% The U of A List of Tables requires a running header on subsequent
%% pages that we must define here.  We also add this section to the TOC.
%% This was modified from the definition for listoffigures from 
%% 2001/04/21 v1.4e book.cls.
%%
\renewcommand{\listoftables}{%
% This bracket begins an unnmaed environment in which we will temporarily
% override the document's page style and margins
{
\renewcommand{\baselinestretch}{1}	% single spaced here
\small\normalsize					% size switch to make the spacing behave

    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
	% vspace added to account for the TOC, LOF, & LOT running header business.
    \chapter*{\vspace*{-0.5in}\listtablename}%
\vspace*{-0.5in}

	% \markboth{\protect\raisebox{-0.5in}{\textbf{\emph{\listtablename} -- Continued}\hspace{1.5in}} }{
	% 		  \protect\raisebox{-0.5in}{\hspace{1.5in}\textbf{\emph{\listtablename} -- Continued}} }
	\markboth{\protect\raisebox{-0.5in}{{\emph{\listtablename} -- Continued}\hspace{1.5in}} }{
			  \protect\raisebox{-0.5in}{\hspace{1.5in}{\emph{\listtablename} -- Continued}} }

	\addcontentsline{toc}{chapter}{\listtablename}
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
}

%\clearpage
%\addtolength{\headsep}{-0.5in}		% Un-adjust things for the running
%\addtolength{\textheight}{0.5in}	% headers for TOC, LOF, LOT.
}

%% ----------------------------------------------------------------------------










%% List of Figures ------------------------------------------------------------
%% 
%% The U of A List of Figures requires a running header on subsequent
%% pages that we must define here.  We also add this section to the TOC.
%% This was modified from the definition for listoffigures from 
%% 2001/04/21 v1.4e book.cls.
%%
\renewcommand{\listoffigures}{%
% This bracket begins an unnmaed environment in which we will temporarily
% override the document's page style and margins
{
\renewcommand{\baselinestretch}{1}	% single spaced here
\small\normalsize					% size switch to make the spacing behave

    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
	% vspace added to account for the TOC, LOF, & LOT running header business.
    \chapter*{\vspace*{-0.5in}\listfigurename}%
\vspace*{-0.5in}

	% \markboth{\protect\raisebox{-0.5in}{\textbf{\emph{\listfigurename} -- Continued}\hspace{1.5in}} }{
	% 		  \protect\raisebox{-0.5in}{\hspace{1.5in}\textbf{\emph{\listfigurename} -- Continued}} }
	\markboth{\protect\raisebox{-0.5in}{{\emph{\listfigurename} -- Continued}\hspace{1.5in}} }{
			  \protect\raisebox{-0.5in}{\hspace{1.5in}{\emph{\listfigurename} -- Continued}} }

  \addcontentsline{toc}{chapter}{\listfigurename}
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi


}
}

%% ----------------------------------------------------------------------------









%% INCLUDE REFERENCES ---------------------------------------------------------
% This is is used to include the reference section after the 
% appendix. There is a .Rmd file associated with it.

\newcommand{\increferences}[1]{

\chapter*{}
\addcontentsline{toc}{chapter}{REFERENCES}

% Indent after first line of each entry
\setlength{\parindent}{-0.2in}
\setlength{\leftskip}{0.2in}
\setlength{\parskip}{8pt}

\InputIfFileExists{#1}
  {\typeout{Including your file #1.tex for the references.}}
  {Couldn't find file.}

}

%% ----------------------------------------------------------------------------
