#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP records_to_dataframe(SEXP Srecord, SEXP Sfield_name, SEXP Svalue, 
                          SEXP Sfield_list, SEXP Sdf, SEXP Sfactors)
{
   SEXP Sfield,Sfield_value;
   const char *record=NULL, *prior_record=NULL;
   const char *field_name, *value;
   int i, row_num = -1;
   Rboolean factors = LOGICAL(Sfactors)[0];

   for (i = 0; i < LENGTH(Srecord); i++)
   {
      record = CHAR(STRING_ELT(Srecord,i));
      field_name = CHAR(STRING_ELT(Sfield_name,i));
      value = CHAR(STRING_ELT(Svalue,i));

      if (prior_record==NULL || strcmp(record,prior_record)!=0)
      {
         row_num++;
         prior_record = record;
      }

      /* Empty string */
      if (value[0] == '\0')
         continue;

      Sfield_value = findVarInFrame(Sfield_list,install(field_name));
      if (Sfield_value==R_UnboundValue) 
      {
         /*warning("Field %s does not exist in project_data",field_name);*/
         continue;
      }
      Sfield = VECTOR_ELT(Sdf,INTEGER(Sfield_value)[0]-1);


      switch(TYPEOF(Sfield))
      {
         case STRSXP:
            SET_STRING_ELT(Sfield,row_num,mkChar(value));
            break;
         case REALSXP:
            REAL(Sfield)[row_num] = asReal(STRING_ELT(Svalue,i));
            break;
         case INTSXP:
            if (factors)
            {
               int val = asInteger(STRING_ELT(Svalue,i));
               int offset = INTEGER(Sfield_value)[1];
               INTEGER(Sfield)[row_num] = INTEGER(Sfield_value)[offset+val];
            }
            else
            {
               INTEGER(Sfield)[row_num] = asInteger(STRING_ELT(Svalue,i));
            }
            break;
         default:
            warning("Field type for %s is %d!",field_name, TYPEOF(Sfield));
            break;
      }
   }

   return Sdf;
}

#define CALLDEF(name, n) {#name,(DL_FUNC) &name, n}
static R_CallMethodDef CallEntries[] = {
   CALLDEF(records_to_dataframe,6),
   {NULL,NULL,0}
};

void R_init_redcapAPI(DllInfo *dll)
{
   R_registerRoutines(dll,NULL,CallEntries, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
}
