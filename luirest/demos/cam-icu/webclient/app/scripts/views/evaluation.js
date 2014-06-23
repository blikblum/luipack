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
                attributes: ['date', 'rass', 'deliriumid', 'sedationid', 'ventilationid'],
                extend: {
                    date:{
                      onGet: function(val){
                          return fromOADate(val).toLocaleDateString();
                      }
                    },
                    deliriumid: {
                        selectOptions:{
                            collection: [{name: 'Hipoativo', value: 1},{name: 'Misto', value: 2},{name: 'Hiperativo', value: 3},{name: 'Não', value: 4},{name: 'Não Avaliado', value: 5}],
                            labelPath: 'name',
                            defaultOption: {label: 'NC', value: 9}
                        }
                    },
                    ventilationid: {
                        selectOptions:{
                            collection: function() {
                               return [
                                    {name: 'Mecânica', value: 1},
                                    {name: 'Espontânea', value: 2},
                                    {name: 'VNI', value: 3}
                                ];
                            },
                            labelPath: 'name',
                            defaultOption: {label: 'NC', value: 9}
                        }
                    },
                    sedationid: {
                        selectOptions:{
                            collection: [{name: 'Propofol', value: 1},{name: 'Midazolam', value: 2},{name: 'Midazolan + Fentanil', value: 23},
                                {name: 'Fentanil', value: 3},{name: 'Cetamina', value: 4},{name: 'Morfina', value: 5},{name: 'Dexmetomedina', value: 6},
                                {name: 'Nada', value: 7}
                            ],
                            labelPath: 'name',
                            defaultOption: {label: 'NC', value: 9}
                        }
                    }
                }
            });
            _.extend(bindings, {
                '.title-el':{
                    observe: 'id',
                    onGet: function (val) {
                        var title = (this.model.isNew()) ? 'Adicionar': 'Editar';
                        title += ' Avaliação';
                        return title;
                    }
                }
            })

            return bindings;
        } ,

        patientBindings: {
          '.name-el':'name'
        },

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
            this.$el.html(this.html);
            this.stickit();
            this.stickit(this.collection.patient, this.patientBindings);
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
                    var msg = '--';
                    console.log('Error saving evaluation', model, response, options);
                    if ((response.responseJSON) && (response.responseJSON.message)){
                        msg = response.responseJSON.message;
                    }
                    alert('Erro ao salvar avaliação: ' + msg);
                }
            })
        }
    });

    return EvaluationView;
});
