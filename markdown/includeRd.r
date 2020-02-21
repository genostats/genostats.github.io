# this is mostly tools::Rd2HTML
# with some modifs to suit my aims
includeRd <- function (Rdfiles, directory, out = "", defines = .Platform$OS.type, 
  Links = NULL, stages = "render", outputEncoding = "UTF-8", 
  dynamic = FALSE, no_links = FALSE, fragment = FALSE, stylesheet = "",
  math = TRUE, sectionLevel = 0L, ...) {

  writeLinesUTF8 <- if(outputEncoding == "UTF-8" || (outputEncoding == "" && l10n_info()[["UTF-8"]])) {
    function(x, con, outputEncoding, ...) writeLines(x, con, useBytes = TRUE, ...)
  } else {
    function(x, con, outputEncoding, ...) {
      x <- iconv(x, "UTF-8", outputEncoding, sub = "byte", mark = FALSE)
      writeLines(x, con, useBytes = TRUE, ...)
    }
  }

  of0 <- function(...) writeLinesUTF8(paste0(...), con, outputEncoding, sep = "")
  of1 <- function(text) writeLinesUTF8(text, con, outputEncoding, sep = "")

  HTMLTags <- c(
    `\\bold` = "b", 
    `\\cite` = "cite", 
    `\\code` = "code", 
    `\\command` = "code", 
    `\\dfn` = "dfn", 
    `\\emph` = "em", 
    `\\kbd` = "kbd", 
    `\\preformatted` = "pre", 
    `\\strong` = "strong", 
    `\\var` = "var")

  HTMLEscapes <- c(
    `\\R` = "<span style=\"font-family: Courier New, Courier; color: #666666;\"><b>R</b></span>", 
    `\\cr` = "<br />", 
    `\\dots` = "...", 
    `\\ldots` = "...")

  HTMLLeft <- c(
    `\\acronym` = "<acronym><span class=\"acronym\">", 
    `\\donttest` = "", 
    `\\env` = "<span class=\"env\">", 
    `\\file` = "&lsquo;<span class=\"file\">", 
    `\\option` = "<span class=\"option\">", 
    `\\pkg` = "<span class=\"pkg\">", 
    `\\samp` = "<span class=\"samp\">", 
    `\\sQuote` = "&lsquo;", 
    `\\dQuote` = "&ldquo;", 
    `\\verb` = "<code style=\"white-space: pre;\">")

  HTMLRight <- c(
    `\\acronym` = "</span></acronym>", 
    `\\donttest` = "", 
    `\\env` = "</span>", 
    `\\file` = "</span>&rsquo;", 
    `\\option` = "</span>", 
    `\\pkg` = "</span>", 
    `\\samp` = "</span>", 
    `\\sQuote` = "&rsquo;", 
    `\\dQuote` = "&rdquo;", 
    `\\verb` = "</code>")

  addParaBreaks <- function(x) {
    if(tools:::isBlankLineRd(x) && isTRUE(inPara)) {
      inPara <<- FALSE
      return("</p>\n")
    }
    start <- attr(x, "srcref")[2L]

    if(start == 1) x <- tools:::psub("^\\s+", "", x)

    if(isTRUE(!inPara) && !all(grepl("^[[:blank:]\n]*$", x, perl = TRUE))) {
        x <- c("<p>", x)
        inPara <<- TRUE
    }
    x
  }

  enterPara <- function(enter = TRUE) {
    if(enter && isTRUE(!inPara)) {
        of0("<p>")
        inPara <<- TRUE
    }
  }

  leavePara <- function(newval) {
    if(isTRUE(inPara)) 
        of0("</p>\n")
    inPara <<- newval
  }

  writeWrapped <- function(tag, block, doParas) {
    if(!doParas || HTMLTags[tag] == "pre") 
      leavePara(NA)
    else 
      enterPara()

    saveAsIs <- inAsIs
    asis <- !is.na(match(tag, "\\command"))
    if(asis) inAsIs <<- TRUE

    if(!tools:::isBlankRd(block)) {
      of0("<", HTMLTags[tag], ">")
      writeContent(block, tag)
      of0("</", HTMLTags[tag], ">")
    }

    if(HTMLTags[tag] == "pre") inPara <<- FALSE
    if(asis) inAsIs <<- saveAsIs
  }

  writeLink <- function(tag, block, doParas) {
    parts <- tools:::get_link(block, tag, Rdfile)

    writeHref <- function() {
      enterPara(doParas)
      savePara <- inPara
      inPara <<- NA
      if(!no_links) of0("<a href=\"", htmlfile, "\">")
        writeContent(block, tag)
      if(!no_links) of1("</a>")
      inPara <<- savePara
    }

    # si on a fourni un vecteur de liens "seuls autorisés"
    # on filtre, sinon on lie par défaut à #dest (ancre supposée dans la page)
    if(!is.null(Links)) {
      if( !(parts$dest %in% Links) ) 
        return()
    }
    htmlfile <- paste0("#", parts$dest)
    writeHref()
  }

  writeLR <- function(block, tag, doParas) {
    enterPara(doParas)
    of1(HTMLLeft[tag])
    writeContent(block, tag)
    of1(HTMLRight[tag])
  }

  writeDR <- function(block, tag) {
    if(length(block) > 1L) {
      of1("## Not run: ")
      writeContent(block, tag)
      of1("\n## End(Not run)")
    } else {
      of1("## Not run: ")
      writeContent(block, tag)
    }
  }

  writeBlock <- function(block, tag, blocktag) {
    doParas <- !(blocktag %in% c("\\tabular"))
    switch(tag, 
        UNKNOWN = , 
        VERB = of1(tools:::vhtmlify(block, inEqn)), 
        RCODE = of1(tools:::vhtmlify(block)), 
        TEXT = of1(if(doParas && !inAsIs) addParaBreaks(tools:::htmlify(block)) else tools:::vhtmlify(block)), 
        USERMACRO = , 
        `\\newcommand` = , 
        `\\renewcommand` = , 
        COMMENT = { }, 
        LIST = writeContent(block, tag), 
        `\\describe` = , 
        `\\enumerate` = , 
        `\\itemize` = {
            leavePara(FALSE)
            writeContent(block, tag) 
        }, 
        `\\bold` = , 
        `\\cite` = , 
        `\\code` = , 
        `\\command` = , 
        `\\dfn` = , 
        `\\emph` = , 
        `\\kbd` = , 
        `\\preformatted` = , 
        `\\strong` = , 
        `\\var` = writeWrapped(tag, block, doParas), 
        `\\special` = writeContent(block, tag), 
        `\\linkS4class` = , 
        `\\link` = writeLink(tag, block, doParas), 
        `\\email` = if(length(block)) {
            url <- paste(as.character(block), collapse = "")
            url <- gsub("\n", "", url)
            enterPara(doParas)
            of0("<a href=\"mailto:", tools:::urlify(url), "\">", tools:::htmlify(url), "</a>")
        }, 
        `\\url` = if(length(block)) {
            url <- paste(as.character(block), collapse = "")
            url <- trimws(gsub("\n", "", url, fixed = TRUE, useBytes = TRUE))
            enterPara(doParas)
            of0("<a href=\"", tools:::urlify(url), "\">", tools:::htmlify(url), "</a>")
        }, 
        `\\href` = {
            if(length(block[[1L]])) {
              url <- paste(as.character(block[[1L]]), collapse = "")
              url <- trimws(gsub("\n", "", url, fixed = TRUE, useBytes = TRUE))
              enterPara(doParas)
              of0("<a href=\"", tools:::urlify(url), "\">")
              closing <- "</a>"
            } else closing <- ""
            savePara <- inPara
            inPara <<- NA
            writeContent(block[[2L]], tag)
            of0(closing)
            inPara <<- savePara
        }, 
        `\\Sexpr` = of0(as.character.Rd(block, deparse = TRUE)), 
        `\\cr` = , 
        `\\dots` = , 
        `\\ldots` = , 
        `\\R` = {
            enterPara(doParas)
            of1(HTMLEscapes[tag])
        }, 
        `\\acronym` = , 
        `\\donttest` = , 
        `\\env` = , 
        `\\file` = , 
        `\\option` = , 
        `\\pkg` = , 
        `\\samp` = , 
        `\\sQuote` = , 
        `\\dQuote` = , 
        `\\verb` = writeLR(block, tag, doParas), 
        `\\dontrun` = writeDR(block, tag), 
        `\\enc` = writeContent(block[[1L]], tag), 
        `\\eqn` = {
            enterPara(doParas)
            if(math) {
              of1("$"); 
              block <- lapply(block[[1]], function(bl) gsub("[[:space:]]+$", "", gsub("^[[:space:]]+", "", bl)) )
              writeContent(block, tag)
              of1("$")
            } else {
              inEqn <<- TRUE
              of1("<i>")
              block <- block[[length(block)]]
              writeContent(block, tag)
              of1("</i>")
              inEqn <<- FALSE
            }
        }, 
        `\\deqn` = {
            if(math) {
              of1("$$");
              block <- lapply(block[[1]], function(bl) gsub("[[:space:]]+$", "", gsub("^[[:space:]]+", "", bl)) )
              writeContent(block, tag)
              of1("$$") 
            } else {
              inEqn <<- TRUE
              leavePara(TRUE)
              of1("<p style=\"text-align: center;\"><i>")
              block <- block[[length(block)]]
              writeContent(block, tag)
              of0("</i>")
              leavePara(FALSE)
              inEqn <<- FALSE
            }
        }, 
        `\\figure` = {
            enterPara(doParas)
            if(dynamic) of1("<img src=\"figures/") else of1("<img src=\"../help/figures/")
            writeContent(block[[1]], tag)
            of1("\" ")
            if(length(block) > 1L && length(imgoptions <- .Rd_get_latex(block[[2]])) && startsWith(imgoptions, "options: ")) {
              imgoptions <- gsub("\\%", "%", imgoptions, fixed = TRUE)
              of1(sub("^options: ", "", imgoptions))
            } else {
              of1("alt=\"")
              writeContent(block[[length(block)]], tag)
              of1("\"")
            }
            of1(" />")
        }, 
        `\\dontshow` = , 
        `\\testonly` = { }, 
        `\\method` = , 
        `\\S3method` = , 
        `\\S4method` = { }, 
        `\\tabular` = writeTabular(block), 
        `\\subsection` = writeSection(block, tag), 
        `\\if` = , 
        `\\ifelse` = if(testRdConditional("html", block, Rdfile)) 
             writeContent(block[[2L]], tag) 
           else if(tag == "\\ifelse") 
             writeContent(block[[3L]], tag), 
        `\\out` = for (i in seq_along(block)) of1(block[[i]]), 
        stopRd(block, Rdfile, "Tag ", tag, " not recognized")
     )
  }

  writeTabular <- function(table) {
    format <- table[[1L]]
    content <- table[[2L]]
    if(length(format) != 1 || tools:::RdTags(format) != "TEXT") 
      stopRd(table, Rdfile, "\\tabular format must be simple text")
    format <- strsplit(format[[1L]], "", fixed = TRUE)[[1L]]
    if(!all(format %in% c("l", "c", "r"))) 
      stopRd(table, Rdfile, "Unrecognized \\tabular format: ", table[[1L]][[1L]])
    format <- c(l = "left", c = "center", r = "right")[format]
    tags <- tools:::RdTags(content)
    leavePara(NA)
    of1("\n<table summary=\"Rd table\">\n")
    newrow <- TRUE
    newcol <- TRUE
    for (i in seq_along(tags)) {
      if(newrow) {
        of1("<tr>\n ")
        newrow <- FALSE
        col <- 0
     }
     if(newcol) {
       col <- col + 1L
       if(col > length(format)) 
         stopRd(table, Rdfile, "Only ", length(format), " columns allowed in this table")
       of0("<td style=\"text-align: ", format[col], ";\">")
       newcol <- FALSE
     }
     switch(tags[i], 
       `\\tab` = {
          of1("</td>")
          newcol <- TRUE
       }, 
       `\\cr` = {
          if(!newcol) of1("</td>")
            of1("\n</tr>\n")
            newrow <- TRUE
            newcol <- TRUE
      },  
      writeBlock(content[[i]], tags[i], "\\tabular"))
    }

    if(!newcol) of1("</td>")
    if(!newrow) of1("\n</tr>\n")
    of1("\n</table>\n")
    inPara <<- FALSE
  }

  writeContent <- function(blocks, blocktag) {
    inlist <- FALSE
    itemskip <- FALSE
    tags <- tools:::RdTags(blocks)
    i <- 0
    while (i < length(tags)) {
      i <- i + 1
      tag <- tags[i]
      block <- blocks[[i]]
      if(length(pendingOpen)) {
        if(tag == "RCODE" && startsWith(block, "(")) {
          block <- sub("^\\(", "", block)
          arg1 <- sub("[,)[:space:]].*", "", block)
          block <- sub(paste0(arg1, "[[:space:]]*,[[:space:]]*"), "", block)
          of0(arg1, pendingOpen)
          if(pendingOpen == "$") 
             pendingClose <<- ""
          else pendingClose <<- chartr("[", "]", pendingOpen)
        } else of0("`", pendingOpen, "`")
        pendingOpen <<- character()
      }
      if(length(pendingClose) && tag == "RCODE" && grepl("\\)", block)) {
        of0(sub("\\).*", "", block), pendingClose)
        block <- sub("[^)]*\\)", "", block)
        pendingClose <<- character()
      }
      switch(tag, 
        `\\method` = , 
        `\\S3method` = , 
        `\\S4method` = {
            blocks <- transformMethod(i, blocks, Rdfile)
            tags <- tools:::RdTags(blocks)
            i <- i - 1
        }, 
        `\\item` = {
            leavePara(FALSE)
            if(!inlist) {
              switch(blocktag, 
                `\\value` = of1("<table summary=\"R valueblock\">\n"), 
                `\\arguments` = of1("<table summary=\"R argblock\">\n"), 
                `\\itemize` = of1("<ul>\n"), 
                `\\enumerate` = of1("<ol>\n"), 
                `\\describe` = of1("<dl>\n")
              )
              inlist <- TRUE
            } else {
              if(blocktag %in% c("\\itemize", "\\enumerate")) {
                of1("</li>\n")
                itemskip <- TRUE
              }
            }
            switch(blocktag, 
              `\\value` = , 
              `\\arguments` = {
                of1("<tr valign=\"top\"><td><code>")
                inPara <<- NA
                writeContent(block[[1L]], tag)
                of1("</code></td>\n<td>\n")
                inPara <<- FALSE
                writeContent(block[[2L]], tag)
                leavePara(FALSE)
                of1("</td></tr>")
            }, 
            `\\describe` = {
                of1("<dt>")
                inPara <<- NA
                writeContent(block[[1L]], tag)
                of1("</dt><dd>")
                inPara <<- FALSE
                writeContent(block[[2L]], tag)
                leavePara(FALSE)
                of1("</dd>")
            }, 
            `\\enumerate` = , 
            `\\itemize` = {
               inPara <<- FALSE
               of1("<li>")
            }
          )
        }, 
        # default 
        {
          if(inlist && !(blocktag %in% c("\\itemize", "\\enumerate")) && !(tag == "TEXT" && tools:::isBlankRd(block))) {
            switch(blocktag, 
              `\\arguments` = , 
              `\\value` = of1("</table>\n"), 
              `\\describe` = of1("</dl>\n"))
              inlist <- FALSE
              inPara <<- FALSE
          } 
          if(itemskip) {
            itemskip <- FALSE
            if(tag == "TEXT") {
              txt <- addParaBreaks(tools:::htmlify(block))
              of1(txt)
            } else writeBlock(block, tag, blocktag)
          } else writeBlock(block, tag, blocktag)
        }
      )
    }
    if(inlist) {
      leavePara(FALSE)
      switch(blocktag, 
          `\\value` = , 
          `\\arguments` = of1("</table>\n"), 
          `\\itemize` = of1("</li></ul>\n"), 
          `\\enumerate` = of1("</li></ol>\n"), 
          `\\describe` = of1("</dl>\n")
      )
    }
  }


  writeSection <- function(section, tag) {
    if(tag %in% c("\\alias", "\\concept", "\\encoding", "\\keyword")) 
      return()
    leavePara(NA)
    save <- sectionLevel
    sectionLevel <<- sectionLevel + 1L
    of1(paste0("\n\n<h", sectionLevel + 1L, ">"))
    if(tag == "\\section" || tag == "\\subsection") {
      title <- section[[1L]]
      section <- section[[2L]]
      writeContent(title, tag)
    } else of1(tools:::sectionTitles[tag])

    of1(paste0("</h", sectionLevel + 1L, ">\n\n"))

    if(tag %in% c("\\examples", "\\usage")) {
      of1("<pre>")
      inPara <<- NA
      pre <- TRUE
    } else {
      inPara <<- FALSE
      pre <- FALSE
    }

    if(length(section)) {
      s1 <- section[[1L]][1L]
      if(tools:::RdTags(section)[1] == "TEXT" && s1 == "\n") 
        section <- section[-1L]
      writeContent(section, tag)
    }
    leavePara(FALSE)
    if(pre) of0("</pre>\n")
    sectionLevel <<- save
  }

  if(is.character(out)) {
    if(out == "") {
      con <- stdout()
    } else {
      con <- file(out, "wt")
      on.exit(close(con))
    }
  } else {
    con <- out
    out <- summary(con)$description
  }


  Rdfiles <- paste0(sub("\\.Rd$", "", Rdfiles), ".Rd")
  for(Rd in file.path(directory, Rdfiles)) {
 
    pendingClose <- pendingOpen <- character()
    inEqn <- FALSE
    inPara <- FALSE
    inAsIs <- FALSE

    Rd <- tools:::prepare_Rd(Rd, defines = defines, stages = stages, fragment = fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- tools:::RdTags(Rd)

    if(fragment) {
      if(sections[1L] %in% names(sectionOrder)) 
        for (i in seq_along(sections)) writeSection(Rd[[i]], sections[i])
      else for (i in seq_along(sections)) writeBlock(Rd[[i]], sections[i], "")
    } else {
      name <- tools:::htmlify(Rd[[2L]][[1L]])
      headtitle <- tools:::.Rd_format_title(tools:::.Rd_get_title(Rd))

      # Ecrit le titre [avec une ancre id = name]

      of1(paste0("\n<h", sectionLevel + 1L, " id=\"", name, "\">"))
      of0("`", name, "`: ", tools:::htmlify(headtitle))
      of1(paste0("</h", sectionLevel + 1L, ">\n\n"))

      # CECI AUSSI ECRIT LE TITRE !!!
      # of1("<h2>")
      # inPara <- NA
      # title <- Rd[[1L]]
      # writeContent(title, sections[1])
      # of1("</h2>")

      inPara <- FALSE
      for (i in seq_along(sections)[-(1:2)]) writeSection(Rd[[i]], sections[i])
  
      of0("\n")
    }
    invisible(out)
  }
}
