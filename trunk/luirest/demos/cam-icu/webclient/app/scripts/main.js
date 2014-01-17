/*global require*/
'use strict';

require.config({
    shim: {
        underscore: {
            exports: '_'
        },
        backbone: {
            deps: [
                'underscore',
                'jquery'
            ],
            exports: 'Backbone'
        },
        bootstrap: {
            deps: ['jquery'],
            exports: 'jquery'
        },
        handlebars: {
            exports: 'Handlebars'
        }
    },
    paths: {
        jquery: '../bower_components/jquery/jquery',
        backbone: '../bower_components/backbone/backbone',
        underscore: '../bower_components/underscore/underscore',
        bootstrap: '../bower_components/sass-bootstrap/dist/js/bootstrap',
        handlebars: '../bower_components/handlebars/handlebars'
    }
});

var app = {};
app.data = {}
app.BASE_URL = '../../luirest/camicu.cgi';
app.mainView = null;
app.setMainView = function(newView){
  if (app.mainView) {
    app.mainView.remove()
  }
  app.mainView = newView;
  $('#main').append(newView.$el);
}

require([
    'jquery',
    'backbone',
  'collections/patients',
  'routes/main'
], function ($, Backbone, PatientCollection, MainRouter) {
   $(document).ready(function(){
     app.data.patients = new PatientCollection();
     app.data.patients.fetch({reset: true});
     app.mainRouter = new MainRouter();
     Backbone.history.start();
     app.mainRouter.navigate('#patients', {trigger:true})
   })

});
