/*global define*/

define([
    'jquery',
    'underscore',
    'backbone',
    'text!templates/evaluation.html',
    'stickitform'

], function ($, _, Backbone, html, StickitForm) {
    'use strict';

    var EvaluationView = Backbone.View.extend({
        html: html,

        bindings: function(){
            var bindings = StickitForm.getBindings({
                attributes: ['date', 'rass', 'deliriumid', 'sedationid', 'ventilationid']
            });
            return bindings;
        } ,

        tagName: 'div',

        id: '',

        className: '',

        events: {
            'click button.save-model': 'saveModel',
            'click button.cancel': 'cancel'
        },

        initialize: function () {
            this.listenTo(this.model, 'change', this.render);
        },

        render: function () {
            var title = (this.model.isNew()) ? 'Adicionar': 'Editar';
            title += ' Avaliação';
            this.$el.html(this.html);
            this.$('.title-el').html(title);
            this.stickit();
            return this;
        },

        cancel: function () {
            //window.history.back();
            app.mainRouter.navigate('#patients/' + this.collection.patient.get('id') + '/evaluations', true)
        },

        saveModel: function () {
            var self = this;
            var isNew = this.model.isNew();
            if (isNew){
                this.model.collection = this.collection;
            };
            this.model.save({}, {
                success: function(model, response, options){
                    console.log('Evaluation saved', model, response, options);
                    if (isNew) {
                        self.collection.add(model);
                    };
                    app.mainRouter.navigate('#patients/' + model.get('patientid') + '/evaluations', true);
                },
                error: function(model, response, options){
                    console.log('Error saving evaluation', model, response, options);
                    alert('Erro ao salvar avaliação');

                }
            })
        }
    });

    return EvaluationView;
});
