#' elaborator_plot_qual_trends - creates the qualitative trends plot in the shiny app elaborator
#'
#' @param dat1 data set
#' @param Variab variable
#' @param fontsize font size
#' @param percent percentage
#' @param method method
#' @param color_palette color vector
#' @param Summa summarization matrix
#'
#' @keywords internal

elaborator_plot_qual_trends <- function(dat1,
                      Variab,
                      fontsize = 1,
                      percent = 0.1,
                      method = "InQuRa",
                      color_palette = c('#ffffff', '#ffffcc', '#ffeda0', '#fed976', '#feb24c', '#fd8d3c',
                                        '#fc4e2a', '#e31a1c', '#bd0026', '#800026', '#000000'),
                      Summa = Summa){

  treat <- ColorFont <- . <- variable <- data <- di <- diff2 <- V <- M <- facdi <- InQuRa <- Range <- refRange <- NULL
  ColorBG <- "#E2F3F2"

  if (length((unique(dat1$TRTP))) == 0 |
      length(unique(dat1[, "PARAMCD"])) == 0) {
    on_ex <- graphics::par("mfrow","bty","mar","oma","bg")
    on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(1,1),
        bty = "n",
        mar = c(1, 1, 1, 1),
        oma = c(0, 0, 0, 0),
        bg = ColorBG)
    graphics::plot(NULL, xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
    graphics::rect(xleft = graphics::grconvertX(0,'ndc','user'), xright = graphics::grconvertX(1, 'ndc', 'user'),
         ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1, 'ndc', 'user'),
         border  =NA, col = ColorBG, xpd = TRUE)
    graphics::text(0.5, 0.5, paste0("No Reference Values available"), col = ColorFont, cex = 1)
  } else {

    shiny::withProgress(message = 'elaborating ...', value = 0, {
      shiny::incProgress(0, detail = paste(""))

      D <- data.frame(subj = dat1$SUBJIDN,
                     treat = dat1$TRTP,
                     variable = dat1$PARAMCD,
                     time = dat1$AVISIT,
                     value = dat1$LBORRES)

      Treats <- levels(D$treat)

      H <- reshape2::dcast(D, treat + variable + subj ~ time)

      longH <- H %>%
        dplyr::group_by(treat, variable) %>%
        tidyr::nest() %>%
        dplyr::mutate(diff = purrr::map(data, ~ .[, colSums(is.na(.)) != nrow(.)])) %>%
        dplyr::mutate(di = purrr::map(diff, ~ dim(.)[2] - 1)) %>%
        dplyr::mutate(diff2 = purrr::map(diff, ~ stats::na.omit(.))) %>%
        dplyr::select(-c(data, diff)) %>%
        dplyr::mutate(facdi = di %>%
                 unlist)  %>%
        dplyr::mutate(V = purrr::map(diff2, ~ .[, -c(1, 2)] - .[ ,-c(1, ncol(.))]))

      longH <- suppressWarnings(dplyr::full_join(longH, Summa, by = c("variable"))) %>%
        dplyr::mutate(M = V)

      longH <- longH %>%
        dplyr::filter(!is.na((treat)))

      if (method == "InQuRa") {
        for(i in 1:(longH %>%
                    dim() %>%
                    .[1])){
          longH[i,]$M[[1]][longH[i,]$V[[1]]  < -longH[i,]$InQuRa] <- -1
          longH[i,]$M[[1]][longH[i,]$V[[1]] >=  -longH[i,]$InQuRa & longH[i,]$InQuRa  >= longH[i,]$V[[1]]] <- 0
          longH[i,]$M[[1]][longH[i,]$V[[1]] > longH[i,]$InQuRa] <- 1
        }
      } else if (method == "Range") {
        for(i in 1:(longH %>%
                    dim() %>%
                    .[1])){
          longH[i,]$M[[1]][longH[i,]$V[[1]]  < -longH[i,]$Range] <- -1
          longH[i,]$M[[1]][longH[i,]$V[[1]] >= -longH[i,]$Range & longH[i,]$Range  >= longH[i,]$V[[1]]] <- 0
          longH[i,]$M[[1]][longH[i,]$V[[1]] > longH[i,]$Range] <- 1
        }
      } else if (method == "Reference Range") {
        for(i in 1:(longH %>%
                    dim() %>%
                    .[1])){
          if (is.na(longH[i,]$refRange)) {
            longH[i,]$M[[1]] <- matrix(NA, dim(longH$M[[1]])[1], dim(longH$M[[1]])[2])
          } else {
            longH[i,]$M[[1]][longH[i,]$V[[1]]  < -longH[i,]$refRange] <- -1
            longH[i,]$M[[1]][longH[i,]$V[[1]] >= -longH[i,]$refRange & longH[i,]$refRange  >= longH[i,]$V[[1]]] <- 0
            longH[i,]$M[[1]][longH[i,]$V[[1]] > longH[i,]$refRange] <- 1
          }
        }
      }

      longH <- longH %>%
        dplyr::mutate(mg = purrr::map(M, ~ rowSums(.)))

      longH <- longH %>%
        dplyr::mutate(mz = purrr::map(M, ~ elaborator_calculate_pattern_number(.)))

      layout(matrix(1:(length(Treats) * length(Variab)),length(Treats),length(Variab)))

      on_ex <- graphics::par("mai", "xaxs", "yaxs", "bg", "fg",
                             "font", "font.lab", "font.main",
                             "font.sub", "font.main", "font.sub",
                             "ps", "cex", "family")

      on.exit(graphics::par(on_ex))

      graphics::par(mai = rep(0, 4), xaxs = "i", yaxs = "i",
          bg = ColorBG,
          fg = grDevices::rgb(140, 140, 140, maxColorValue = 255),
          font = 1, font.axis = 1, font.lab = 1, font.main = 1, font.sub = 1,
          ps = 5, cex = 1,
          family = "sans")


      for (va in Variab){
        for (tr in Treats){
          if(all(is.na(longH %>%
                       dplyr::filter(variable == va , treat == tr) %>%
                       .$M %>%
                       unlist()))){
            graphics::plot(NULL, xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
            graphics::rect(xleft=graphics::grconvertX(0,'ndc','user'), xright=graphics::grconvertX(1,'ndc','user'),
                 ybottom=graphics::grconvertY(0,'ndc','user'), ytop=graphics::grconvertY(1,'ndc','user'),
                 border=NA,col = ColorBG, xpd=TRUE)
            graphics::text(0.5,0.5,paste0("No Reference Values available in ",va), col = ColorFont, cex = 1)
          }else{

            k <- longH %>%
              dplyr::filter(variable == va , treat == tr) %>%
              dplyr::pull(facdi)

            m = k - 1
            n = 3 ** m
            mz = 0:(n - 1)

            L = lapply(mz,function(i) elaborator_calculate_pattern(i,m))

            E = matrix(unlist(L),n,m,byrow=TRUE)

            colnames(E)=paste("M",1:(k-1),sep="")

            mg = unlist(lapply(1:n,function(i) sum(E[i,])))

            s=matrix("=",n,m); s[E < 0]="-"; s[E > 0]="+"
            s=apply(s,1,paste,collapse=" ")

            E=data.frame(mz,E,s,mg)
            E=data.frame( E[order(E$mg,decreasing=TRUE),] ,h=rep(0,n),r=rep(0,n),mp=1:n)

            Z = E
            b <- longH %>%
              dplyr::filter(variable == va , treat == tr)
            if(b$mg %>%
               unlist() %>%
               length() > 0){
              z <- b$mz %>%
                unlist() %>%
                table()
              q = as.numeric(names(z))

              Z$h[elaborator_perform_unlist(q,Z$mz)]=z
              Z$r=round(100*Z$h/sum(Z$h))
            }

            w=floor(max(as.vector(by(E$mg,E$mg,length)))/2)+1
            graphics::plot(-w:w,-w:w,ylim=c(min(E$mg)-1,max(E$mg)+1),xlab="", ylab="",type="n",xaxs="i",yaxs="i",axes=FALSE)
            graphics::rect(xleft=graphics::par()$usr[1]-0.1, ybottom=graphics::par()$usr[3]-0.1, xright=graphics::par()$usr[2]+0.1, ytop=graphics::par()$usr[4]+0.1,col=ColorBG) #ColorPanel

            if(method == "InQuRa"){
              graphics::text(graphics::par()$usr[1] + 2, graphics::par()$usr[4] - 0.2, paste0("Tolerated difference:",
                                                                Summa %>%
                                                                  dplyr::filter(variable == va) %>%
                                                                  dplyr::pull(InQuRa) %>%
                                                                  round(2)), cex = fontsize)
            }else if(method == "Range"){
              graphics::text(graphics::par()$usr[1] + 2, graphics::par()$usr[4] - 0.2, paste0("Tolerated difference:",
                                                                Summa %>%
                                                                  dplyr::filter(variable == va) %>%
                                                                  dplyr::pull(Range) %>%
                                                                  round(2)), cex = fontsize)
            }else if(method == "Reference Range"){
              graphics::text(graphics::par()$usr[1] + 2, graphics::par()$usr[4] - 0.2, paste0("Tolerated difference:",
                                                                Summa %>%
                                                                  dplyr::filter(variable == va) %>%
                                                                  dplyr::pull(refRange) %>%
                                                                  round(2)), cex = fontsize)
            }

            tlx=0
            for (i in max(E$mg):min(E$mg)){

              q=Z[Z$mg==i,];

              l=dim(q)[1]; hl=floor(l/2)


              xl=(-hl):(-1); xr=1:hl

              x=if (l %% 2 == 0) c(xl,xr) else c(xl,0,xr)

              tlx=ifelse(min(x) < tlx,min(x),tlx)

              for (j in 1:l){
                graphics::rect(x[j]-0.5,i-0.5,x[j]+0.5,i+0.5, col = color_palette[elaborator_calculate_color_index(q$r[j])])
                if (q$h[j] > 0){
                  f=ifelse(q$r[j] < 30,"black","white")
                  if(fontsize!=0){
                    graphics::text(x[j],i+0.3,q$h[j],col=f, cex = fontsize)
                    graphics::text(x[j],i+0.0,paste(q$r[j],"%",sep=""),col=f, cex = fontsize)
                    graphics::text(x[j],i-0.3,q$s[j],col=f, cex = fontsize)
                  }
                }
              }
            }

            if (tr == Treats[1]){
              graphics::text(x[j],max(E$mg)+0.75,va,cex=2)
            }

            if (va == Variab[1]){
              graphics::text(tlx-0.75,(min(E$mg)+max(E$mg))/2,tr,srt=90,cex=2)
            } }

          shiny::incProgress(1/(length(Variab)*length(Treats)), detail = paste(""))
        }
      }
      shiny::incProgress(0, detail = paste("done!"))
    })
  }
}
