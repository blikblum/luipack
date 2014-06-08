/*global define*/

define([
    'jquery',
    'underscore',
    'backbone',
    'templates',
    'vendor/jquery.serialize-object'
], function ($, _, Backbone, JST, FormSerializer) {
    'use strict';

    var PatienteditView = Backbone.View.extend({
      template: JST['app/scripts/templates/patientedit.hbs'],
      initialize:function(){


      },
      render: function () {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
      },
      events:{
        'click button.save-model':'saveModel',
        'click button.cancel':'cancel'
      },
      cancel: function () {
        app.mainRouter.navigate('#patients', {trigger:true});
      },
      saveModel: function(){
        var self = this;
        var saved = false;
        var data = this.$('form').serializeObject();
        this.model.set(data);
        if (this.model.isNew()){
          this.model.collection = this.collection;
          this.model.save({
            success: function(){
              saved = true;
              this.collection.add(this.model);
            }
          });

        } else {
          this.model.save({
            success: function(){
              saved = true;
            }
          });
        }
        if (saved) {
          app.mainRouter.navigate('#patients', {trigger: true});
        } else{
          alert('Erro ao salvar os dados');
        }
      }
    });

    return PatienteditView;
});
