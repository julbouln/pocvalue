#include <libxml/parser.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>


int validate (xmlDocPtr doc)
{
  int r;
  xmlValidCtxtPtr cvp;
  cvp = xmlNewValidCtxt();
  cvp->userData = (void *) stderr;
  cvp->error    = (xmlValidityErrorFunc) fprintf;
  cvp->warning  = (xmlValidityWarningFunc) fprintf;
  r=xmlValidateDocument(cvp, doc);
  xmlFreeValidCtxt(cvp);
  return r;
}

value xml_xinclude_process_file(value filename)
{
  xmlDocPtr doc;
  int options=0;
  xmlChar *result;
  int len;


  options |= XML_PARSE_XINCLUDE;
  //  options |= XML_PARSE_DTDVALID;

  doc=xmlReadFile((xmlChar *)String_val(filename),NULL,options);

  xmlXIncludeProcessFlags(doc, options);
  //validate(doc);

  xmlDocDumpMemory(doc, &result, &len);
  

  xmlFreeDoc(doc);
  return (copy_string(result));
}



value xml_xinclude_process_string(value str,value url)
{
  xmlDocPtr doc;
  int options=0;
  xmlChar *result;
  int len;
  options |= XML_PARSE_XINCLUDE;

  doc=xmlReadMemory((const char *)String_val(str),string_length(str),(xmlChar *)String_val(url),NULL,options);

  xmlXIncludeProcessFlags(doc, options);

  xmlDocDumpMemory(doc, &result, &len);
  

  xmlFreeDoc(doc);
  return (copy_string(result));
}
