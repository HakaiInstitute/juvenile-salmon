---
title: Juvenile Salmon Migration Dynamics in the Discovery Islands and Johnstone Strait in 2018
preprint: false
author: 
  - name: Brett T. Johnson
    affiliation: 1
    corresponding: true
    email: brett.johnson@hakai.org
  - name: Julian C.L. Gan
  - name: Brian P.V. Hunt
    affiliation: 2, 3
affiliation:
  - code: 1
    address: Hakai Institute Quadra Island Ecological Observatory, Heriot Bay, BC V0P1H0
  - code: 2
    address: UBC EOS, IOF
abstract: >
  The majority of out-migrating juvenile Fraser River salmon pass northwest through the Strait of Georgia, the Discovery Islands, and Johnstone Strait. The Discovery Islands to Johnstone Strait leg of the migration is a region of poor survival for juvenile salmon relative to the Strait of Georgia. The Hakai Institute Juvenile Salmon Program has been monitoring key components of this migration since 2015 to better understand drivers of early marine survival. Here we present key aspects of the 2018 migration in comparison to averages from the 2015—2018 study period, which we use to define 'normal'. In 2018 sockeye, pink, and chum all migrated earlier than normal. The median capture date was May 23rd for sockeye, five days earlier than normal, and June 12 for pink and chum which is five days earlier for pink and three days earlier than normal for chum. Sea lice prevalence was lower than normal for sockeye, pink, and chum. Notably there were no _Lepeoptheirus salmonis_ sea lice observed in Johnstone Strait in 2018. Sockeye were longer than normal in 2018 whereas pink and chum were smaller than normal. Sea surface temperature in May and June was the warmest on record in the study period (2015—2018). Pink salmon dominated the catch in 2018, followed by chum and then sockeye. 
header-includes: >
  \usepackage{lipsum}
bibliography: sample.bib
output: 
  rticles::peerj_article:
    base_format: bookdown::pdf_document2 # for using \@ref()
---


```r
# All data used for this analysis is from the hakaisalmon R package v0.2.0 at https://hakaiinstitute.github.io/hakaisalmon/
library(hakaisalmon)
library(tidyverse)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```
## Warning: package 'purrr' was built under R version 3.4.4
```

```
## Warning: package 'dplyr' was built under R version 3.4.4
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.4.4
```

```r
library(knitr)
library(here)
```


# Introduction {-}

Pacific salmon (_Oncorhynchus_ spp.) traverse a number of aquatic landscapes during different phases of their lifecyle to reach a habitat which ultimately offers some reward. While undergoing these migrations, salmon are subjected to risks associated with each new environemnt they encounter. The risks and associated mortality from these migrations can be understood in aggregate by quantifying the productivity (recruits per spawner) of a certain stock. Salmon are an excellent indicator species beacuse they act as an integrator of terrestrial, lacustrine, fluvial, estuarine, nearshore marine, and high-seas conditions. A problem in any one of these environments will be reflected in the productivity of a salmon stocks. To better manage and predict the producitivty of salmon stocks we need estimates of mortality and an understanding of the factors driving mortality in each landscape that salmon traverse. The early marine environment is one which estimates of mortality and it's drivers, is lacking. Juvenile salmon are particularly vulnerable during the early marine phase of their life history because they are undergoing physiological adaptations to a saline environment. 

The Hakai Institute Juvenile Salmon Program has been monitoring juvenile salmon migrations in the Discovery Islands and Johnstone Strait (Figure \@ref(fig:map)) 2015 in an effort to understand what factors may be influencing early marine survival of sockeye, pink, and chum (Hunt et al. 2018). The effects of pathogens, parasites, predators, and the impacts of climate change on food web dynamics may be amplified during this stressful transition period and are the primary aspects of the salmon migration we are monitoring and reporting on here.


```r
include_graphics('map.pdf')
```

\begin{figure}

\includegraphics{map} \hfill{}

\caption{Sampling locations in 2018}(\#fig:map)
\end{figure}

## About PeerJ {-}

PeerJ is an award-winning open access publisher covering the biological and medical sciences.  PeerJ provides authors with three publication venues: *PeerJ* and *PeerJ Computer Science* (peer-reviewed academic journals) and *PeerJ PrePrints* (a 'pre-print server'). See https://peerj.com/about/publications/ for more information.

The PeerJ model allows an author to publish articles in their peer-reviewed journal via the purchase of a lifetime Publication Plan. Prices start from just \$99 (a one-off payment) which entitles an author to the lifetime ability to publish 1 article per year for free. Publication in PeerJ PrePrints is entirely free.

# Some \LaTeX{} Examples {-}

Use section and subsection commands to organize your document. \LaTeX{} handles all the formatting and numbering automatically. Use ref and label commands for cross-references.

## Figures and Tables {-}

Use the table and tabular commands for basic tables --- see Table \@ref(tab:widgets), for example. You can upload a figure (JPEG, PNG or PDF) using the project menu. To include it in your document, use the includegraphics command as in the code for Figure \@ref(fig:view) below.

Standard \LaTeX references will work as well (e.g. Fig. \ref{fig:view}).

<!-- There are three ways to include figures: -->
<!-- 1. The LaTeX way -->
<!-- 

\begin{figure}[ht]
\centering
\includegraphics[width=\linewidth]{view}
\caption{An example image.}
\label{fig:view}
\end{figure} 

 -->

<!-- 2. The RMarkdown way -->

\begin{figure}
\includegraphics[width=1\linewidth]{view} \caption{An example image.}(\#fig:view)
\end{figure}


<!-- 3. The pandoc way -->
<!-- 

![An example image. Text *can* be processed with markdown.](view.pdf){#view width = 100%}

 -->

<!-- 
\begin{table}[ht]
\centering
\begin{tabular}{l|r}
Item & Quantity \\\hline
Widgets & 42 \\
Gadgets & 13
\end{tabular}
\caption{\label{tab:widgets}An example table.}
\end{table}
-->

Item     Quantity
------- ---------
Widgets        42
Gadgets        13

Table: (\#tab:widgets) An Example Table.

## Citations {-}

LaTeX formats citations and references automatically using the bibliography records in your .bib file, which you can edit via the project menu. Use the cite command for an inline citation, like @Figueredo:2009dg, and the citep command for a citation in parentheses [@Figueredo:2009dg].

## Mathematics {-}

\LaTeX{} is great at typesetting mathematics. Let $X_1, X_2, \ldots, X_n$ be a sequence of independent and identically distributed random variables with $\text{E}[X_i] = \mu$ and $\text{Var}[X_i] = \sigma^2 < \infty$, and let
$$S_n = \frac{X_1 + X_2 + \cdots + X_n}{n}
      = \frac{1}{n}\sum_{i}^{n} X_i$$
denote their mean. Then as $n$ approaches infinity, the random variables $\sqrt{n}(S_n - \mu)$ converge in distribution to a normal $\mathcal{N}(0, \sigma^2)$.

## Lists {-}

You can make lists with automatic numbering \dots


1. Like this,
1. and like this.

or bullet points...

- Like this,
- and like this.

or with descriptions...

- **Word** Definition
- **Concept** Explanation
- **Idea** Text


We hope you find write\LaTeX\ useful for your PeerJ submission, and please let us know if you have any feedback. Further examples with dummy text are included in the following pages.

# Methods {-}

\lipsum[4] 

\begin{equation}
\cos^3 \theta =\frac{1}{4}\cos\theta+\frac{3}{4}\cos 3\theta
\label{eq:refname2}
\end{equation}

\lipsum[5] 

## Subsection {-}

\lipsum[6] 

\paragraph{Paragraph} \lipsum[7] 
\paragraph{Paragraph} \lipsum[8] 

## Subsection {-}

\lipsum[9] 

<!-- \begin{figure}[ht]\centering -->
<!-- \includegraphics[width=\linewidth]{results} -->
<!-- \caption{In-text Picture} -->
<!-- \label{fig:results} -->
<!-- \end{figure} -->

\begin{figure}
\includegraphics[width=1\linewidth]{Migration_Dynamics_PeerJ_files/figure-latex/results-1} \caption{In-text Picture}(\#fig:results)
\end{figure}


Reference to Figure \@ref(fig:results).

# Results and Discussion {-}

\lipsum[10] 

## Subsection {-}

\lipsum[11] 

### Subsubsection {-}

\lipsum[12] 

### Subsubsection {-}

\lipsum[14] 

## Subsection {-}

\lipsum[15-20] 

# Acknowledgments {-}

So long and thanks for all the fish.

# References