define(['handlebars'], function(Handlebars) {

this["JST"] = this["JST"] || {};

this["JST"]["app/scripts/templates/patient.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  var buffer = "", stack1, functionType="function", escapeExpression=this.escapeExpression;


  buffer += "<td>";
  if (stack1 = helpers.name) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.name; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "</td>\n<td></td>\n<td>\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "\"><span class=\"glyphicon glyphicon-edit\"></span></a>\n  <a href=\"#/patients/";
  if (stack1 = helpers.id) { stack1 = stack1.call(depth0, {hash:{},data:data}); }
  else { stack1 = depth0.id; stack1 = typeof stack1 === functionType ? stack1.apply(depth0) : stack1; }
  buffer += escapeExpression(stack1)
    + "/evaluations\"><span class=\"glyphicon glyphicon-list-alt\"></span></a>\n</td>\n\n\n";
  return buffer;
  });

this["JST"]["app/scripts/templates/patients.hbs"] = Handlebars.template(function (Handlebars,depth0,helpers,partials,data) {
  this.compilerInfo = [4,'>= 1.0.0'];
helpers = this.merge(helpers, Handlebars.helpers); data = data || {};
  


  return "<div class=\"panel panel-default\">\n  <div class=\"panel-heading\">\n    <strong class=\"panel-title\">Pacientes</strong>  <a id=\"add-patient\" class=\"btn btn-primary btn-sm pull-right\"><span class=\"glyphicon glyphicon-plus-sign\"></span> Admitir Paciente</a>\n  </div>\n  <div class=\"panel-body\">\n    <table id=\"patients-table\" class=\"table table-hover\">\n      <thead>\n      </thead>\n      <tbody>\n      </tbody>\n    </table>\n  </div>\n</div>\n\n";
  });

return this["JST"];

});