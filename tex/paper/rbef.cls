%%%%
% rbef.cls
% Copyright (c) Youssef Cherem, 2016
% Consider the following:
% Considere o seguinte:
% License: http://creativecommons.org/licenses/by-nc-sa/4.0/
% (CC BY-NC-SA 4.0)
% Attribution — You must give appropriate credit, provide a link to the
% license, and indicate if changes were made. You may do so in any
% reasonable manner, but not in any way that suggests the licensor
% endorses you or your use.
%%%%%% No warranties are given.%%%%%%%

% NonCommercial — You may not use the material for commercial purposes.

% ShareAlike — If you remix, transform, or build upon the material, you
% must distribute your contributions under the same license as the
% original.

% Adaptação do modelo RBEF para o TCC do Curso de Especialização em Data
% Science \& Big Data da UFPR. A versão original está disponível em:
% https://pt.overleaf.com/articles/rbef-particula-carregada/kbbhpyygstft.

%-----------------------------------------------------------------------
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{rbef}[2016/04/11]

\LoadClass[a4paper, twoside, DIV=calc, headings=normal, numbers=endperiod, parskip=never]{scrartcl}

\addtokomafont{subtitle}{\small}
\addtokomafont{author}{\fontsize{12}{14}}

\renewcaptionname{brazil}{\abstractname}{\vspace{-28pt}}
\renewcaptionname{english}{\abstractname}{\vspace{-28pt}}

\RequirePackage{mathtools}%substitui e acrescenta funções a amsmath
% amssymb internally loads amsfonts!
\RequirePackage{amssymb} %para \mathbb{text}
\RequirePackage{mathrsfs} % para mathscr

%-----------------------------------------------------------------------
% fontes e compilação com pdflatex

\RequirePackage{ifxetex}

\ifxetex
\RequirePackage{fontspec} % fontes
%\defaultfontfeatures{Ligatures=TeX} %not needed anymore?
\setmainfont[Numbers=OldStyle]{Minion Pro}
\setsansfont{Myriad Pro}
\setmonofont[Scale=MatchLowercase]{Myriad Pro}
%\RequirePackage{polyglossia} %polyglossia retirado: 22/12/2015
%\setmainlanguage{brazil}
%\setotherlanguages{english,french,spanish,german,italian}
\RequirePackage[english,french,spanish,es-tabla,brazil]{babel}
\RequirePackage[euler-digits,euler-hat-accent,T1]{eulervm} %fonte matemática, retirada a pedido do editor, 21/12/2015
\let\mathbf\mathbold
% se quiser fontes diferentes para matemática (Times/Asana):
%\RequirePackage[mathbf=sym]{unicode-math}
%\setmathfont{Asana Math}%[version=asana]
%\setmathfont{XITS Math}
%\setmathfont[range={\mathcal,\mathbfcal},StylisticSet=1]{XITS Math} % also for \mathscr

\else
\RequirePackage[utf8]{inputenc}
\addtokomafont{disposition}{\rmfamily}
\RequirePackage[english, brazil]{babel}
%\RequirePackage{lmodern}
\RequirePackage{fourier} % much better for screen AND printing.
\RequirePackage[lining, scale=0.9]{FiraSans} % why use Latin Modern Sans?
\firalining
\RequirePackage[scale=.9]{FiraMono}
\fi

\RequirePackage{textcomp}

\RequirePackage{microtype} % melhor justificação de parágrafos
\RequirePackage{scrlayer-scrpage} %antigo scrpage2
\RequirePackage{graphicx} % para inserção de figuras
\RequirePackage[svgnames, dvipsnames]{xcolor} % cores
\RequirePackage{sidecap}
\widowpenalty=10000
\clubpenalty=10000
\setlength{\parskip}{0pt}
%\setheadsepline{.4pt}
%\addtokomafont{caption}{\footnotesize}
\setcapindent{0pt}
%\RequirePackage{pdflscape}
%\RequirePackage{pdfpages}
\RequirePackage[outer=2cm,inner=1.3cm,bottom=3cm,top=2.5cm]{geometry}
\RequirePackage{lipsum}
\pagestyle{scrheadings} % estilo de cabeçalho

\RequirePackage[stable,bottom]{footmisc}% para footnotes em títulos. pôr footnote DENTRO. % bottom para resolver problema com footnote + figura no fim de página
\deffootnote[0.3em]{0em}{0em} %notas de rodapé, footnotes
{\textsuperscript{\thefootnotemark}}

%%%%%% ----------CABEÇALHO--------------%%%%%%%%%%%
%%%%%%%==============================
\ofoot[]{\scriptsize \rbef, \volumerevista, \numerorevista, \numerocabecalho, \anorevista}
% \ifoot[\scriptsize Copyright by Sociedade Brasileira de Física. Printed in Brazil.]{\scriptsize \doirevista}
\ifoot[
\scriptsize Especialização em Data Science \& Big Data $\cdot$ Universidade Federal do Paraná
]{\scriptsize \doirevista}
%\ohead{\pagemark}
\chead{}
\rehead[]{\titleheader}
\lohead[]{\authorheader}
\lehead[]{\numerocabecalho-\pagemark}
\rohead[]{\numerocabecalho-\pagemark}%[\pagemark] entre colchetes: entre colchetes, plain.scrheadings (plain: primeira página)

%%%%=================================================

%\automark[section]{section}
\RequirePackage{wrapfig}
%\setkomafont{headsepline}{\color{BurntOrange}} % cor da linha que separa cabeçalho %% BurntOrange MidnightBlue JungleGreen
\setkomafont{pagehead}{\normalfont \small}
\setkomafont{pagefoot}{\normalfont \small}
%\RequirePackage{caption} % really necessary?
\setkomafont{caption}{\small \sffamily}
\setkomafont{captionlabel}{\small \sffamily \bfseries}
\RequirePackage[normalem]{ulem}
\RequirePackage[framemethod=tikz]{mdframed}

%% Para quadros com fundo em equações e outros elementos
%\surroundwithmdframed[backgroundcolor=Lavender]{equation}
\surroundwithmdframed[backgroundcolor=Lavender]{verbatim}
%\surroundwithmdframed[backgroundcolor=Lavender]{displaymath}
%\surroundwithmdframed[backgroundcolor=Lavender]{align}
%\surroundwithmdframed[backgroundcolor=Lavender]{align*}

%\RequirePackage{etoolbox}
%
%% add the environment to \[\] (needs etoolbox)
%\preto{\[}{%
%	\begin{mdframed}[backgroundcolor=Lavender]%
%	\vspace{-\baselineskip}%
%}
%\appto{\]}{%
%\end{mdframed}%
%}

\RequirePackage{booktabs} %tabelas profissionais
\RequirePackage{tabularx} % para facilitar nas tabelas
\RequirePackage{longtable}
\RequirePackage{ccicons} % ícones creative commons
\RequirePackage{multicol}
\RequirePackage[bookmarks=true,breaklinks=true]{hyperref}
\g@addto@macro{\UrlBreaks}{\UrlOrds} % incluído em 04-04-2016
\RequirePackage[switch,columnwise]{lineno}
\RequirePackage[blocks]{authblk} %blocks: always author/affiliation blocks
\renewcommand\Affilfont{\small} %fonte afiliação. %\itshape
\renewcommand\Authand{, } % separação entre penúltimo e último autor no título
\renewcommand\Authands{, } % separação entre autores no título

\RequirePackage{abstract}%%[runin] -- %para abstract e \saythanks

%%% bibliografia fonte menor
\AfterBibliographyPreamble{\small}

%%% para diminuir espaço entre entradas na bibliografia
\let\OLDthebibliography\thebibliography
\renewcommand\thebibliography[1]{
  \OLDthebibliography{#1}
  \setlength{\parskip}{0pt}
  \setlength{\itemsep}{0pt plus 0.3ex}
}


%%%% uso econômico de espaços%%%%

\renewcommand\floatpagefraction{.9}
\renewcommand\dblfloatpagefraction{.9} % for two column documents
\renewcommand\topfraction{.9}
\renewcommand\dbltopfraction{.9} % for two column documents
\renewcommand\bottomfraction{.9}
\renewcommand\textfraction{.1}
\setcounter{totalnumber}{50}
\setcounter{topnumber}{50}
\setcounter{bottomnumber}{50}

\setlength{\columnsep}{18pt} %espaço entre colunas
\setlength{\footskip}{36pt} % espaço entre texto e rodapé

\date{\vspace{-5ex}}

%% PARTE DE INSERÇÃO DE TEXTO -- AUTORES -- EXEMPLO
%	\author[1]{Fulano de Tal}
%	\author[2]{Beltrano Aristides Quadrilátero}
%	\affil[1]{Instituto de Geologia, Universidade do Triângulo de Ferro.%\thanks{\href{emailto:fulano@ferro.com}{fulano@ferro.com}}
% }
%   \affil[2]{Universidade do outro autor.}
%
%%%%%%%%%%%%%%%%%%%%

%\providecommand{\imprimirtipotrabalho}{}
%\newcommand{\tipotrabalho}[1]{\renewcommand{\imprimirtipotrabalho}{#1}}

%put empty command in .cls
\newcommand{\volumerevista}{}
\newcommand{\volume}[1]{\renewcommand{\volumerevista}{vol. #1}}

\newcommand{\numerorevista}{}
\newcommand{\numero}[1]{\renewcommand{\numerorevista}{nº #1}}

\newcommand{\anorevista}{}
\newcommand{\ano}[1]{\renewcommand{\anorevista}{#1}}

\newcommand{\doirevista}{}
% \newcommand{\doi}[1]{\renewcommand{\doirevista}{DOI: #1}}
\newcommand{\doi}[1]{\renewcommand{\doirevista}{#1}}

\newcommand{\titleheader}{}
\newcommand{\titulocabecalho}[1]{\renewcommand{\titleheader}{#1}}

\newcommand{\authorheader}{}
\newcommand{\autorcabecalho}[1]{\renewcommand{\authorheader}{#1}}

\newcommand{\numerocabecalho}{}
\newcommand{\numeracao}[1]{\renewcommand{\numerocabecalho}{#1}}

\newcommand{\tipodeartigotexto}{}
\newcommand{\tipodeartigo}[1]{\renewcommand{\tipodeartigotexto}{#1}}

% \newcommand{\rbef}{Revista Brasileira de Ensino de Física}
\newcommand{\rbef}{Trabalho de Conclusão de Curso}

\titlehead{\small \sffamily
  \begin{minipage}{0.6\textwidth}

    \begin{minipage}{8.5em}
      \includegraphics[width=8em]{dsbd1x4.png}
    \end{minipage}
    \begin{minipage}{20em}
    \tipodeartigotexto

    \url{dsbd.leg.ufpr.br}
  \end{minipage}

  \end{minipage}
  \hfill
  \begin{minipage}{0.4\textwidth}

    \flushright
    Universidade Federal do Paraná

    \rbef,
    % \volumerevista,
    \numerorevista,
    \numerocabecalho~(\anorevista) %(ano em parênteses, tire parênteses se quiser)

  \end{minipage}

  \vspace{1ex}
  \hrule{}
  % \hspace*{24pt}
}

% \addtocounter{page}{566} %% \setcounter produces extra white page!!! use ===\addtocounter===

\frenchspacing

\renewcommand{\date}{\vspace{-5ex}}

\newcommand{\titulo}[1]{\title{\LARGE #1}}
\let\subtitulo\subtitle

\newcommand{\palavraschave}[1]{\newline\textbf{Palavras-chave:} #1}
\newcommand{\keywords}[1]{\newline\textbf{Keywords:} #1}
\newcommand{\palavrasclave}[1]{\newline\textbf{Palavras clave:} #1}
%

%% não dá um salto de página, mas acaba com numeração e prejudica todo o resto:
\newenvironment{primeirapagina}{\begin{strip}\vspace*{-2cm}\maketitle}{\end{strip}}%\vspace*{-2cm} added on 30-12-15

%%%COPIED FROM THE PACKAGE WIDETEXT
%%%\NeedsTeXFormat{LaTeX2e}
%\ProvidesPackage{widetext}
%% Mimics the widetext environment of revtex4 for any other class package
%% Eg: article.cls
%%
%% Compiled by: Anjishnu Sarkar <<==================
%%
%% Advantages:
%% *) Supports float (eg: figure) in two column format (Advantage over
%%      multicol package)
%% *) One and twocolumn exist on the same page
%% *) Flow of text shown via rule
%% *) Equal height of text when in two column format
%%
%% Acknowledgment(s):
%% 1. Instead of re-inventing the wheel, two packages (flushend, cuted) of
%% the sttools bundle are used. The sttools bundle is available from CTAN.
%% Lisence of these packages rests with their corresponding author.
%% Any bug/problem with flushend and cuted should be forwarded to their
%% corresponding package authors.
%% 2. The idea of the rule came from the following latex community website
%% http://www.latex-community.org/forum/viewtopic.php?f=5&t=2770
%%
%% This package just defines the widetext environment and the rules.
%%
%% Usage:
%% \documentclass[a4paper,12pt,twocolumn]{article}
%% \usepackage{widetext}
%%
%% \begin{document}
%%
%% Some text in twocolumn
%%
%% \begin{widetext}
%% Text in onecolumn format.
%% \end{widetext}
%%
%% Some more text in twocolumn
%%
%% \end{document}
%%%%%%%%%%%%%%%%%%%%

%% Package required for equal height while in 2 columns format
\IfFileExists{flushend.sty}
{\RequirePackage{flushend}}
{\typeout{}
  \typeout{Package widetext error: Install the flushend package which is
    a part of sttools bundle. Available from CTAN.}
  \typeout{}
  \stop
}

%% Package required for onecolumn and twocolumn to exist on the same page.
%% and also required for widetext environment.
%\IfFileExists{cuted.sty}
%{
\RequirePackage[unskipbreak,autobase]{cuted} %unskipbreak and autobase correct "disappearing text" when widetext is used on the right column, but leave a space.
%}
%{\typeout{}
%	\typeout{Package widetext error: Install the cuted package which is
%		a part of sttools bundle. Available from CTAN.}
%	\typeout{}
%	\stop
%}

\newlength\@parindent
\setlength\@parindent{\parindent}

\if@twocolumn
\newenvironment{widetext}
{%
  \par \vspace{-\baselineskip} % <-- this line added
  \begin{strip}
    \rule{\dimexpr(0.5\textwidth-0.5\columnsep-0.4pt)}{0.4pt}%
    \rule{0.4pt}{6pt}
    \par %\vspace{6pt}
    \parindent \@parindent
  }%
  {%
    \par
    \hfill\rule[-6pt]{0.4pt}{6.4pt}%
    \rule{\dimexpr(0.5\textwidth-0.5\columnsep-1pt)}{0.4pt}
  \end{strip}
  \par \vspace{-\baselineskip}\noindent % <-- this line added
  \hspace*{-\parindent} % <-- this line added
}
\else
\newenvironment{widetext}{\rule{\dimexpr(0.5\textwidth-0.5\columnsep-0.4pt)}{0.4pt}%
  \rule{0.4pt}{6pt}
  \par %\vspace{6pt}
  \parindent \@parindent}%
{\par
  \hfill\rule[-6pt]{0.4pt}{6.4pt}%
  \rule{\dimexpr(0.5\textwidth-0.5\columnsep-1pt)}{0.4pt}
  \par \vspace{-\baselineskip}\noindent % <-- this line added
  \hspace*{-\parindent} % <-- this line added}
}
\fi

%%%%% two make your life easier using multicols %%%%%%%%%%%%%

\newcommand{\duascolunas}{\begin{multicols}{2}}
  \newcommand{\umacoluna}{\end{multicols}}
\let\comeco\duascolunas
\let\fim\umacoluna

%%% DO PACOTE dblfloatfix

%% This is the LaTeX package dblfloatfix which solves two problems:
%%   a)~floats in a twocolumn document come out in the right order and
%%   b)~allowed float positions are now [tbp].
%%
%% It may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License, either version 1.3c
%% of this license or (at your option) any later version.
%% The latest version of this license is in
%%    http://www.latex-project.org/lppl.txt
%\ProvidesPackage{dblfloatfix}[2012/12/31 v1.0a (JAW)]
%%
%% USAGE:
%%
%% This package redefines internals of the LaTeX kernel by merging
%% the code of the packages fixltx2e (the fix2col part) and stfloats.
%%
%% If either of the packages stfloats or fix2col are loaded you will
%% get an error message and your document will probably not be able
%% to compile. So don't load them!
%%
%% When the new output routine for LaTeX3 is done, this package will
%% be obsolete. The sooner the better...
%%
%% If you're using the subfigure package this package will provide a
%% fix for \end@dblfloat by Steven Cochran (the author of subfigure).
%%
%% Morten H\o gholm, <moho0...@student.cbs.dk>
%\RequirePackage{fixltx2e}
%% An error message for those who didn't read the instructions.
\newcommand*{\DBLFIXerrormessage}[1]{%
  \PackageError{dblfloatfix}{%
    Don't use the #1 package!^^J%
    Didn't you read the instructions in dblfloatfix.sty?!^^J%
    You're in big trouble now...^^J%
    Quit immediately (type `X') and correct the mistake}%
  {Type `X' dammit!}%
}
%% A check for those users who don't read manuals.
\AtBeginDocument{%
  \@ifpackageloaded{stfloats}{\DBLFIXerrormessage{stfloats}}{}%
  \@ifpackageloaded{fix2col}{\DBLFIXerrormessage{fix2col}}{}%
  %% Here's a fix by Steven Douglas Cochran (the author of subfigure).
  %% It is needed in order to make subfigure work with this package as
  %% both packages redefine \end@dblfloat.
  \@ifpackageloaded{subfigure}{%
    \typeout{Redefining \string\end@dblfloat\ for use with subfigure}%
  \def\end@dblfloat{%
  \if@twocolumn
  \@listsubcaptions{\@captype}%
  \@endfloatbox
  \ifnum\@floatpenalty <\z@
  \@largefloatcheck
  \global\dp\@currbox1sp %
  \expandafter\@gobble\subfig@end@float
  \fi
  \else
\end@float
\fi}%
}{}%
}
%% Here we go...
\def\@dblbotlist{}
\newcount\@dblbotnum
\newcounter{dblbotnumber}
\setcounter{dblbotnumber}{2}
\def\@dbflt#1{\@ifnextchar[{\@xdblfloat{#1}}{\@xdblfloat{#1}[tbp]}}

\def \@dblfloatplacement {%
  \global \@dbltopnum \c@dbltopnumber
  \global \@dblbotnum \c@dblbotnumber
  \global \@dbltoproom \dbltopfraction\@colht
  \@textmin \@colht
  \advance \@textmin -\@dbltoproom
  \@fpmin \dblfloatpagefraction\textheight
  \@fptop \@dblfptop
  \@fpsep \@dblfpsep
  \@fpbot \@dblfpbot
  \def\f@depth{1sp}%
}

\def\@addtodblcol{%
  \begingroup
  \@insertfalse
  \@setfloattypecounts
  \@getfpsbit \tw@
  \ifodd\@tempcnta
  \@flsetnum \@dbltopnum
  \ifnum \@dbltopnum>\z@
  \@tempswafalse
  \ifdim \@dbltoproom>\ht\@currbox
  \@tempswatrue
  \else
  \ifnum \@fpstype<\sixt@@n
  \advance \@dbltoproom \@textmin
  \ifdim \@dbltoproom>\ht\@currbox
  \@tempswatrue
  \fi
  \advance \@dbltoproom -\@textmin
  \fi
  \fi
  \if@tempswa
  \@bitor \@currtype \@deferlist
  \@testwrongwidth\@currbox
  \if@test
  \else
  \@bitor \@currtype \@dblbotlist
  \if@test
  \else
  \@tempdima -\ht\@currbox
  \advance\@tempdima
  -\ifx \@dbltoplist\@empty \dbltextfloatsep
  \else \dblfloatsep
  \fi
  \global \advance \@dbltoproom \@tempdima
  \global \advance \@colht \@tempdima
  \global \advance \@dbltopnum \m@ne
  \@cons \@dbltoplist \@currbox
  \@inserttrue
  \fi
  \fi
  \fi
  \fi
  \fi
  \if@insert
  \else
  \@getfpsbit 4\relax
  \ifodd\@tempcnta
  \@flsetnum \@dblbotnum
  \ifnum \@dblbotnum>\z@
  \@tempswafalse
  \ifdim \@dbltoproom>\ht\@currbox
  \@tempswatrue
  \else
  \ifnum \@fpstype<\sixt@@n
  \advance \@dbltoproom \@textmin
  \ifdim \@dbltoproom>\ht\@currbox
  \@tempswatrue
  \fi
  \advance \@dbltoproom -\@textmin
  \fi
  \fi
  \if@tempswa
  \@bitor \@currtype \@deferlist
  % Test for twocolumn floats (2008/08/21 Mattias Nissler)
  \@testwrongwidth\@currbox
  \if@test
  \else
  \@tempdima -\ht\@currbox
  \advance\@tempdima
  -\ifx \@dblbotlist\@empty \dbltextfloatsep
  \else \dblfloatsep
  \fi
  \global \advance \@dbltoproom \@tempdima
  \global \advance \@colht \@tempdima
  \global \advance \@dblbotnum \m@ne
  \@cons \@dblbotlist \@currbox
  \@inserttrue
  \fi
  \fi
  \fi
  \fi
  \fi
  \if@insert
  \else
  \@cons\@deferlist\@currbox
  \fi
  \endgroup
}

\def\@combinedblfloats{%
  \ifx \@dbltoplist\@empty \else \@cdblflt \fi
  \ifx \@dblbotlist\@empty \else \@cdblflb \fi
}

\def\@cdblflt{%
  \setbox\@tempboxa \vbox{}%
  \let \@elt \@comdblflelt
  \@dbltoplist
  \let \@elt \relax
  \xdef \@freelist {\@freelist\@dbltoplist}%
  \global\let \@dbltoplist \@empty
  \setbox\@outputbox \vbox% to\textheight
  {%\boxmaxdepth\maxdepth   %% probably not needed, CAR
    \unvbox\@tempboxa\vskip-\dblfloatsep
    \ifnum \@dbltopnum>\m@ne
    \dblfigrule
    \fi
    \vskip \dbltextfloatsep
    \unvbox \@outputbox
  }%
}

\def\@cdblflb{%
  \setbox\@tempboxa \vbox{}%
  \let \@elt \@comdblflelt
  \@dblbotlist
  \let \@elt \relax
  \xdef \@freelist {\@freelist\@dblbotlist}%
  \global\let \@dblbotlist \@empty
  \setbox\@outputbox \vbox% to\textheight
  {%\boxmaxdepth\maxdepth   %% probably not needed, CAR
    \unvbox\@outputbox
    \vskip \dbltextfloatsep
    \ifnum \@dblbotnum>\m@ne
    \dblfigrule
    \fi
    \unvbox\@tempboxa
    \vskip -\dblfloatsep
  }%
}

\def \@doclearpage {%
  \ifvoid\footins
  \setbox\@tempboxa\vsplit\@cclv to\z@ \unvbox\@tempboxa
  \setbox\@tempboxa\box\@cclv
  \xdef\@deferlist{\@toplist\@botlist\@deferlist}%
  \global \let \@toplist \@empty
  \global \let \@botlist \@empty
  \global \@colroom \@colht
  \ifx \@currlist\@empty
  \else
  \@latexerr{Float(s) lost}\@ehb
  \global \let \@currlist \@empty
  \fi
  \@makefcolumn\@deferlist
  \@whilesw\if@fcolmade \fi{\@opcol\@makefcolumn\@deferlist}%
  \if@twocolumn
  \if@firstcolumn
  \xdef\@deferlist{\@dbltoplist\@dblbotlist\@deferlist}%
  \global \let \@dbltoplist \@empty
  \global \let \@dblbotlist \@empty
  \global \@colht \textheight
  \begingroup
  \@dblfloatplacement
  \@makefcolumn\@deferlist
  \@whilesw\if@fcolmade \fi{\@outputpage
    \@makefcolumn\@deferlist}%
  \endgroup
  \else
  \vbox{}\clearpage
  \fi
  \fi
  \ifx\@deferlist\@empty \else\clearpage \fi
  \else
  \setbox\@cclv\vbox{\box\@cclv\vfil}%
  \@makecol\@opcol
  \clearpage
  \fi
}

\RequirePackage{multirow} % células em várias linhas em tabelas.
\RequirePackage{enumitem} % enumerate e itemize sem espaço entre itens (ver abaixo:)
\setlist{noitemsep}

%%% hifenização de seções permitida abaixo. descomente para permitir hifenização. Mas quem quer hifenizar seções? Mude de acordo com seu gosto, por sua própria conta e risco.
% \renewcommand*{\raggedsection}{}% default is \raggedright

% mudado 07 01 2016
% \newenvironment{duascolunas}{\vspace*{10pt}\begin{minipage}{\textwidth}
%   \begin{equacaolonga}
%   }
%     {\end{equacaolonga}	\end{minipage}\vspace*{10pt}}
%
\newmdenv[rightline=false,leftline=false,
linecolor=gray!50,linewidth=1.5pt]{equacaolonga}%roundcorner=10pt



% \newenvironment{duascolunas}{\vspace*{10pt}\hspace*{.12\columnwidth}\begin{minipage}{.85\textwidth}
%   \begin{equacaolonga}
%   }
%     {\end{equacaolonga}	\end{minipage}\vspace*{10pt}}
%%
% \newmdenv[rightline=false,leftline=false,
% linecolor=gray!50,linewidth=1.5pt]{equacaolonga}%roundcorner=10pt


%%% Comandos que foram retirados mas que podem ser úteis.
% \newenvironment{duascolunas}{\begin{strip}
%   \begin{widetext}
%   }
%     {\end{widetext}\end{strip}}
%%
% \newmdenv[rightline=false,leftline=false,
% align=center,linecolor=gray]{widetext}%roundcorner=10pt

%%% \begin{strip} %% use this for one-column/widetext space
%%%   \vspace{-28pt}
%%% \end{strip}

\endinput
