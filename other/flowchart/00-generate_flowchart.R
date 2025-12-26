#### Preamble ####
# Purpose: Generates a flowchart of the data ingestion → analysis pipeline
# Author: Mike Cowan
# Date: 15 August 2025
# Contact: m.cowan@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# - Run 00.0-required_packages.R first
# NOTE: need to center box titles

#### Workspace setup ####
# source("./scripts/00.0-required_packages.R")

library(DiagrammeR)
## For PNG export
## (these are optional; if not installed, PNG export will be skipped with a message)
## install via: install.packages(c("DiagrammeRsvg","rsvg"))
## library(DiagrammeRsvg); library(rsvg)

#### Generate flowchart ####
data_pipeline_flowchart <- grViz("
  digraph pipeline {
    graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.9, splines = true, pad = \"0.2,0.2\", bgcolor = white,
           labelloc = \"t\", labeljust = \"c\",
           label = <
             <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"1\" CELLSPACING=\"0\" COLOR=\"#00000000\"> 
               <TR>
                 <TD FIXEDSIZE=\"TRUE\" WIDTH=\"240\"> 
                   <TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLPADDING=\"6\" COLOR=\"#333333\" BGCOLOR=\"lightblue\">
                     <TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
                       <FONT FACE=\"Segoe UI\" POINT-SIZE=\"11\" COLOR=\"#111111\"><B>IJF</B><BR/>(subscriber access)</FONT>
                     </TD></TR>
                   </TABLE>
                 </TD>
                 <TD WIDTH=\"24\"></TD>
                 <TD FIXEDSIZE=\"TRUE\" WIDTH=\"240\"> 
                   <TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLPADDING=\"6\" COLOR=\"#333333\" BGCOLOR=\"#D80621\">
                     <TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
                       <FONT FACE=\"Segoe UI\" POINT-SIZE=\"11\" COLOR=\"white\"><B>Elections</B><BR/>(public access)</FONT>
                     </TD></TR>
                   </TABLE>
                 </TD>
                 <TD WIDTH=\"24\"></TD>
                 <TD FIXEDSIZE=\"TRUE\" WIDTH=\"240\"> 
                   <TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLPADDING=\"6\" COLOR=\"#333333\" BGCOLOR=\"lightgrey\">
                     <TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
                       <FONT FACE=\"Segoe UI\" POINT-SIZE=\"11\" COLOR=\"#111111\"><B>Outputs</B><BR/>(parquet/rds)</FONT>
                     </TD></TR>
                   </TABLE>
                 </TD>
               </TR>
             </TABLE>
           >]
    node  [shape = rectangle, fontname = \"Segoe UI\", fontsize = 11, fontcolor = \"#111111\", color = \"#333333\", penwidth = 1.2, width=2.8, fixedsize=false, labelloc=\"c\", labeljust=\"c\", style = \"rounded,filled\"]
    edge  [arrowhead = normal, arrowsize = 0.6, color = \"#555555\", penwidth = 1.0]

  #### Raw Inputs ####
  raw_ijf [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Procurement Dataset (CSV)</B><BR/>
        <BR/>
        • award_date • posted_date • start_date<BR/>
        • method • total_amount • currency<BR/>
        • country • status • source
      </TD></TR></TABLE>
  >, fillcolor=lightblue]

  raw_elections [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Federal Elections</B><BR/>
        <BR/>
        • writ_start <BR/>
        • election_date (2004–2025)<BR/>
        • government_before_election
      </TD></TR></TABLE>
  >, fillcolor=\"#D80621\", fontcolor=white]

    #### Cleaning ####
  clean_main [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Cleaning</B><BR/>
        <BR/>
        • Clean names; drop UNKNOWN source<BR/>
        • Parse dates; keep ≥ 2004-01-01<BR/>
        • Filter AWARDED; currency (CAD); country (CA, NA)<BR/>
        • Remove negatives in total_amount
      </TD></TR></TABLE>
  >, shape=oval, fillcolor=\"#F2E394\"]

  method_class [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Award Flags</B><BR/>
        <BR/>
        • Competitive: Open, Selective, Limited, Traditional<BR/>
        • Non competitive: Advance, Sole-source
      </TD></TR></TABLE>
  >, shape=oval, fillcolor=\"#F2E394\"]

  field_trim [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Reduce Fields</B><BR/>
        <BR/>
        • Drop IDs, free text, location fields<BR/>
        • Keep: award_date, total_amount, method, competitive
      </TD></TR></TABLE>
  >, shape=oval, fillcolor=\"#F2E394\"]

  build_elections [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Build Elections Dataset</B><BR/>
        <BR/>
        • Enter dates &amp; writ_start<BR/>
        • Label government_before_election
      </TD></TR></TABLE>
  >, shape=oval, fillcolor=\"#F2E394\"]

    #### Stage 1 Outputs ####
  parquet_analysis [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Analysis Dataset</B><BR/>
        <BR/>
        • analysis_dataset.parquet
      </TD></TR></TABLE>
  >, shape=folder, fillcolor=lightgrey, width=2.8]

    parquet_elections [label=<
      <TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"2\"><TR><TD ALIGN=\"CENTER\" VALIGN=\"MIDDLE\">
        <B>Elections Dataset</B><BR/>
        <BR/>
        • elections_dataset.parquet
      </TD></TR></TABLE>
    >, shape=folder, fillcolor=lightgrey, width=2.8]

    #### Visual grouping (clusters) ####
    subgraph cluster_inputs {
      label = <<b>Raw Inputs</b>>; fontname=\"Segoe UI\"; fontsize=12; color=\"#C8C8C8\"; style=\"rounded\"; bgcolor=\"#FAFAFA\";
      raw_ijf; raw_elections;
    }

    subgraph cluster_processes {
      label = <<b>Cleaning & Construction</b>>; fontname=\"Segoe UI\"; fontsize=12; color=\"#C8C8C8\"; style=\"rounded\"; bgcolor=\"#FAFAFA\";
      clean_main; method_class; field_trim; build_elections;
    }

    subgraph cluster_outputs {
      label = <<b>Stage 1 Outputs</b>>; fontname=\"Segoe UI\"; fontsize=12; color=\"#C8C8C8\"; style=\"rounded\"; bgcolor=\"#FAFAFA\";
      parquet_analysis; parquet_elections;
    }

    #### Connections ####
    raw_ijf -> clean_main
    clean_main -> method_class
    method_class -> field_trim
    field_trim -> parquet_analysis

    raw_elections -> build_elections
    build_elections -> parquet_elections

  #### Legend moved to graph label at top ####

    #### Grouping (visual ranks) ####
    { rank=same; raw_ijf; raw_elections }
    { rank=same; clean_main; method_class; field_trim; build_elections }
    { rank=same; parquet_analysis; parquet_elections }
  }
")

#### Display ####
data_pipeline_flowchart

#### Save as .rds ####
# saveRDS(data_pipeline_flowchart, file = "./other/flowchart/data_pipeline_flowchart_update.rds")

#### Save as PNG ####
png_path <- file.path("other", "flowchart", "data_pipeline_flowchart_update.png")

if (requireNamespace("DiagrammeRsvg", quietly = TRUE) && requireNamespace("rsvg", quietly = TRUE)) {
  svg_txt <- DiagrammeRsvg::export_svg(data_pipeline_flowchart)
  rsvg::rsvg_png(
    charToRaw(svg_txt),
    file = png_path,
    width = 2400,
    height = 1200
  )
  message(sprintf("Saved PNG to: %s", png_path))
} else {
  message(
    "PNG export skipped: install.packages(c('DiagrammeRsvg','rsvg')) to enable PNG saving."
  )
}
