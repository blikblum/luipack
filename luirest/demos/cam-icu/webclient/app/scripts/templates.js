define(['handlebars'], function(Handlebars) {

this["JST"] = this["JST"] || {};

this["JST"]["app/scripts/templates/evaluationlistitem.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, stack2, options, helperMissing=helpers.helperMissing, escapeExpression=this.escapeExpression, functionType="function";


  buffer += "<td>";
  options = {hash:{},data:data};
  buffer += escapeExpression(((stack1 = helpers.dateToStr || depth0.dateToStr),stack1 ? stack1.call(depth0, depth0.date, options) : helperMissing.call(depth0, "dateToStr", depth0.date, options)))
    + "</td>\r\n<td></td>\r\n<td>\r\n    <span class=\"glyphicon glyphicon-edit\"></span><a href=\"#patients/";
  if (stack2 = helpers.patientid) { stack2 = stack2.call(depth0, {hash:{},data:data}); }
  else { stack2 = depth0.patientid; stack2 = typeof stack2 === functionType ? stack2.apply(depth0) : stack2; }
  buffer += escapeExpression(stack2)
    + "/evaluations/";
  if (stack2 = helpers.id) { stack2 = stack2.call(depth0, {hash:{},data:data}); }
  else { stack2 = depth0.id; stack2 = typeof stack2 === functionType ? stack2.apply(depth0) : stack2; }
  buffer += escapeExpression(stack2)
    + "\"> Editar</a>\r\n</td>\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patient.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, stack2, options, functionType="function", escapeExpression=this.escapeExpression, helperMissing=helpers.helperMissing;


  buffer += "<td>";
  if (stack1 = helpers.bednumber) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.bednumber; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + " - ";
  if (stack1 = helpers.name) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.name; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "<br/><a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/evaluations\">";
  options = {hash:{},data:data};
  buffer += escapeExpression(((stack1 = helpers.pluralCount || depth0.pluralCount),stack1 ? stack1.call(depth0, depth0.evaluationcount, "avaliação", "avaliações", options) : helperMissing.call(depth0, "pluralCount", depth0.evaluationcount, "avaliação", "avaliações", options)))
    + "</a></td>\r\n<td>\r\n  <span class=\"glyphicon glyphicon-edit\"></span><a href=\"#/patients/";
  if (stack2 = helpers.id) { stack2 = stack2.call(depth0, {hash:{},data:data}); }
  else { stack2 = depth0.id; stack2 = typeof stack2 === functionType ? stack2.apply(depth0) : stack2; }
  buffer += escapeExpression(stack2)
    + "/actions\"> Editar</a> &nbsp\r\n</td>\r\n\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patients.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  


  return "     <div class=\"row\">\r\n        <div class=\"col-md-12\">\r\n            <h3>Pacientes\r\n                <a href=\"#\" class=\"btn btn-primary btn-sm pull-right add-popover\" data-placement=\"bottom\" data-toggle=\"popover\" data-title=\"Insira o registro\" type=\"button\" data-html=\"true\"><span class=\"glyphicon glyphicon-plus-sign\"></span> Adicionar</a>\r\n            </h3>\r\n            <div class=\"hide add-popover-content\">\r\n                <form role=\"form\">\r\n                    <div class=\"form-group\">\r\n                        <input type=\"number\" class=\"form-control patient-registry\" placeholder=\"Registro\">\r\n                    </div>\r\n                    <button type=\"submit\" class=\"btn btn-primary add-patient\">Admitir</button>\r\n                </form>\r\n            </div>\r\n        </div>\r\n    </div>\r\n\r\n    <table id=\"patients-table\" class=\"table table-hover\">\r\n      <thead>\r\n      </thead>\r\n      <tbody>\r\n      </tbody>\r\n    </table>\r\n\r\n";
  });

return this["JST"];

});