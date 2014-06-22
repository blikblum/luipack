define(['handlebars'], function(Handlebars) {

this["JST"] = this["JST"] || {};

this["JST"]["app/scripts/templates/evaluationlistitem.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, functionType="function", escapeExpression=this.escapeExpression;


  buffer += "<td>";
  if (stack1 = helpers.date) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.date; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "</td>\r\n<td></td>\r\n<td>\r\n    <a href=\"#patients/";
  if (stack1 = helpers.patientid) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.patientid; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/evaluations/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "\"><span class=\"glyphicon glyphicon-edit\"></span></a>\r\n</td>\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patient.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, functionType="function", escapeExpression=this.escapeExpression;


  buffer += "<td>";
  if (stack1 = helpers.name) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.name; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "</td>\r\n<td></td>\r\n<td>\r\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/edit\"><span class=\"glyphicon glyphicon-edit\"></span></a>\r\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/evaluations\"><span class=\"glyphicon glyphicon-list-alt\"></span></a>\r\n</td>\r\n\r\n\r\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patients.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  


  return "<div class=\"panel panel-default\">\r\n  <div class=\"panel-heading\">\r\n    <strong class=\"panel-title\">Pacientes</strong> <a href=\"#\" class=\"btn btn-primary btn-sm pull-right add-popover\" data-placement=\"bottom\" data-toggle=\"popover\" data-title=\"Insira o registro\" type=\"button\" data-html=\"true\"><span class=\"glyphicon glyphicon-plus-sign\"></span> Admitir Paciente</a>\r\n    <div class=\"hide add-popover-content\">\r\n      <form class=\"form-inline\" role=\"form\">\r\n        <div class=\"form-group\">\r\n          <input type=\"number\" class=\"form-control patient-registry\" placeholder=\"Registro\">\r\n        </div>\r\n        <button type=\"submit\" class=\"btn btn-primary add-patient\">Admitir</button>\r\n      </form>\r\n    </div>\r\n  </div>\r\n  <div class=\"panel-body\">\r\n    <table id=\"patients-table\" class=\"table table-hover\">\r\n      <thead>\r\n      </thead>\r\n      <tbody>\r\n      </tbody>\r\n    </table>\r\n  </div>\r\n</div>\r\n\r\n";
  });

return this["JST"];

});