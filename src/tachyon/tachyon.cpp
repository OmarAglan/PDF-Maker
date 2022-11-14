#include <tidy.h>
#include <buffio.h>
#include <stdio.h>
#include <errno.h>
int filecounter;

void dumpNode( TidyNode tnod, FILE* pfile, TidyDoc tdoc, int cen ) {
  TidyNode child;
  TidyNode grandchild;
  for ( child = tidyGetChild(tnod); child; child = tidyGetNext(child) ) {
    ctmbstr name;
    switch ( tidyNodeGetType(child) ) {
    case TidyNode_Root:  
    case TidyNode_DocType:  
    case TidyNode_Comment: 
    case TidyNode_ProcIns:   
    case TidyNode_Text:  
    case TidyNode_CDATA: 
    case TidyNode_Section:
    case TidyNode_Asp:
    case TidyNode_Jste: 
    case TidyNode_Php:
    case TidyNode_XmlDecl:
    case TidyNode_End:
    case TidyNode_StartEnd:
       name ="none";break;
    default:
      name = tidyNodeGetName( child );
      break;
    }
    assert( name != NULL );

    TidyBuffer buf;
    tidyBufInit (&buf);
    char c='#';
    if ((tidyNodeGetType(child)==TidyNode_Text)) {
    
          tidyNodeGetText 	( tdoc,	child, &buf) ;
      while(!tidyBufEndOfInput(&buf)){ 
          c=tidyBufGetByte(&buf);

          switch (c) { 
          case '\'': fprintf(pfile,"%s", "\\textquotesingle{}");break;
          case '[' : fprintf(pfile,"%s", "{$\\text{[}$}");break;
          case ']' : fprintf(pfile,"%s", "{$\\text{]}$}");break;
          case '&' : fprintf(pfile,"%s", "\\&");break;
          case '%' : fprintf(pfile,"%s", "\\%");break;
          case '{' : fprintf(pfile,"%s", "\\{");break;
          case '}' : fprintf(pfile,"%s", "\\}");break;
          case '_' : fprintf(pfile,"%s", "\\_");break;
          case '$' : fprintf(pfile,"%s", "\\${}");break;
          case '#' : fprintf(pfile,"%s", "\\#");break;
          case '~' : fprintf(pfile,"%s", "\\~{}");break;
          case '^' : fprintf(pfile,"%s", "\\^{}");break;
          case '"' : fprintf(pfile,"%s", "\\symbol{34}");break;
          case '\\' : fprintf(pfile,"%s", "\\textbackslash{}");break;
          case '<' : fprintf(pfile,"%s", "<{}");break;
          case '>' : fprintf(pfile,"%s", ">{}");break;
          case '-' : fprintf(pfile,"%s", "-{}");break;
          default:  fprintf(pfile, "%c", c);
          }
       }
    }

    TidyAttr attr=tidyAttrGetById (child,TidyAttr_CLASS ) ;	
    ctmbstr val =tidyAttrValue 	(attr	) ;
    TidyAttr idattr=tidyAttrGetById (child,TidyAttr_ID ) ;	
    ctmbstr idval =tidyAttrValue 	(idattr	) ;

    if  ((val!=NULL)&&(!(strcmp (val,"image")))) {
        grandchild = tidyGetChild(child);
        TidyAttr grandattr=tidyAttrGetById (grandchild,TidyAttr_SRC ) ;	
        ctmbstr grandval =tidyAttrValue	(grandattr) ;
        while (0==0) {
           if ((grandval[0]=='.')&&(grandval[1]=='.')&&(grandval[2]=='/')) grandval=grandval+3;
           else break;
        }
        char fn[1000];
        filecounter++;
        sprintf(fn,"./%d",filecounter);
        rename(grandval, fn);
        char cmd[1000];
        sprintf(cmd,"convert %s %s.png",fn,fn);
        system(cmd);
        sprintf(fn,"./%d.png",filecounter);
        fprintf(pfile,"\n\n\\begin{minipage}{1.0\\linewidth}\n\\begin{center}\n\\includegraphics[width=1.0\\linewidth,height=6.5in,keepaspectratio]{%s}\n\\end{center}\n\\raggedright{}\\myfigurewithcaption{%d}{unknown caption}\n\\end{minipage}\\vspace{0.75cm}\n\n",fn,filecounter);
        continue;
    }
    if (!(strcmp (name,"math"))) {

      TidyAttr mattr;
      for ( mattr = tidyAttrFirst(child); mattr; mattr =  tidyAttrNext(mattr) ) {
        ctmbstr mattrname = tidyAttrName(mattr) ;	
        if ((mattrname!=NULL)&&(!(strcmp (mattrname,"alttext")))) {
          ctmbstr mattrval =tidyAttrValue(mattr);
          size_t l=strlen(mattrval);
          if (cen){
            fprintf(pfile,"%s","\\begin{equation*}");
          }
          else {
            fprintf(pfile,"%s","{$");
          }
          for (size_t i=0; i< l;i++) {
            if (mattrval[i]=='\n') {fprintf(pfile,"%s"," ");}
            else {fprintf(pfile,"%c",mattrval[i]);}
          }
          if (cen){
            fprintf(pfile,"%s","\\end{equation*}");
          }
          else {
            fprintf(pfile,"%s","$}");
          }
        } 
      }
      continue;

    }	
    if ((val!=NULL)&&(!(strcmp (val,"mw-editsection")))) continue;
    if ((val!=NULL)&&(!(strcmp (val,"toc")))) continue;
    if ((idval!=NULL)&&(!(strcmp (idval,"mw-navigation")))) continue;
    if ((idval!=NULL)&&(!(strcmp (idval,"footer")))) continue;
    if ((name!=NULL)&&(!(strcmp (name,"script")))) continue;
    if (!(strcmp (name,"h1"))) fprintf(pfile,"%s","\\chapter{");
    if (!(strcmp (name,"h2"))) fprintf(pfile,"%s","\\section{");
    if (!(strcmp (name,"h3"))) fprintf(pfile,"%s","\\subsection{");
    if (!(strcmp (name,"h4"))) fprintf(pfile,"%s","\\subsubsection{");
    if (!(strcmp (name,"h5"))) fprintf(pfile,"%s","\\paragraph{");
    if (!(strcmp (name,"h6"))) fprintf(pfile,"%s","\\subparagraph{");
    if (!(strcmp (name,"b"))) fprintf(pfile,"%s","{\\bfseries ");

    if ((name!=NULL)&&(!(strcmp (name,"center")))){
      dumpNode( child, pfile , tdoc, 1==1);
    }
    else {
      dumpNode( child, pfile , tdoc, cen);
    }
    if (!(strcmp (name,"b"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"p"))) fprintf(pfile,"%s","\n");    
    if (!(strcmp (name,"h1"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"h2"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"h3"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"h4"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"h5"))) fprintf(pfile,"%s","}");
    if (!(strcmp (name,"h6"))) fprintf(pfile,"%s","}");
  }
}



int main(int argc, char **argv )
{
  const char * bab= "\\usepackage[english]{babel}\n\\newcommand{\\mychapterbabel}{Chapter}\n\\newcommand{\\mypagebabel}{on page}\n\\newcommand{\\myfigurebabel}{Figure}\n\\newcommand{\\mylangbabel}{english}";
  filecounter=0;
  FILE * pfile;
  system("cp -r ../../document/headers ../headers");
  pfile =fopen ("../headers/babel.tex","w");
  fprintf(pfile,"%s",bab);
  fclose(pfile);
  pfile =fopen ("../headers/svg.tex","w");
  fprintf(pfile,"%s","\\newcommand{\\SVGExtension}{png}");
  fclose(pfile);
  pfile =fopen ("../headers/paper.tex","w");
  fprintf(pfile,"%s","\\KOMAoption{paper}{A4}");
  fclose(pfile);
  pfile =fopen ("../headers/title.tex","w");
  fprintf(pfile,"%s","\\title{unknown}");
  fclose(pfile);


  system("cp ../../latex/my-head.tex ./myfile.tex");
  pfile =fopen ("myfile.tex","a");
  char str[10000];
  char str2[10000];
  strcpy(str, "wget -E -H -k -p ");
  strcat(str,argv[1]);
  strcpy(str2, (argv[1])+8);
  strcat(str2,".html");
  
  //return 0;
  system(str);
  TidyBuffer errbuf = {0};
   TidyDoc tdoc = tidyCreate(); 
  tidyOptSetBool( tdoc, TidyXhtmlOut, yes );
  tidySetErrorBuffer( tdoc, &errbuf );  
  tidyParseFile(tdoc, str2 ) ;
  tidyCleanAndRepair( tdoc );    
  TidyNode bdy=tidyGetBody (tdoc);
  dumpNode(bdy,pfile,tdoc,1==0);
  tidyBufFree( &errbuf );
  tidyRelease( tdoc );
  fclose(pfile);
  system("cat ../../latex/my-tail.tex >> ./myfile.tex");
  system("xelatex --interaction=nonstopmode myfile.tex >/dev/null"); 
  system("xelatex --interaction=nonstopmode myfile.tex >/dev/null"); 
  system("xelatex --interaction=nonstopmode myfile.tex >/dev/null"); 
  return 0; //g++ -I/usr/include/tidy tachyon.cpp  -ltidy

}
