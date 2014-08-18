CREATE TABLE [AppUser] (
  [UserName] VARCHAR, 
  [PasswordHash] VARCHAR, 
  [PasswordSalt] VARCHAR, 
  [Info] TEXT, 
  [Rights] VARCHAR, 
  CONSTRAINT [sqlite_autoindex_AppUser_1] PRIMARY KEY ([UserName]));


CREATE TABLE [Patient] (
  [Id] INTEGER PRIMARY KEY, 
  [Name] VARCHAR, 
  [Registry] VARCHAR NOT NULL, 
  [BirthDate] DATE, 
  [Gender] CHAR(2), 
  [BedNumber] INTEGER, 
  [OriginationId] INTEGER, 
  [InternmentDate] DATETIME, 
  [InternmentTypeId] INTEGER, 
  [IsReInternment] BOOLEAN, 
  [IsReInternment48h] BOOLEAN, 
  [DiagnosticId] INTEGER, 
  [Apache2] INTEGER, 
  [SAPS3] INTEGER, 
  [DischargeDate] DATE, 
  [DischargeReasonId] INTEGER, 
  [VMDuration] INTEGER, 
  [HasICC] BOOLEAN, 
  [HasIRC] BOOLEAN, 
  [HasDCPF] BOOLEAN, 
  [HasDPOC] BOOLEAN, 
  [HasHematologyTumor] BOOLEAN, 
  [HasLocoregionalTumor] BOOLEAN, 
  [HasMetastasis] BOOLEAN, 
  [HasHAS] BOOLEAN, 
  [HasDM] BOOLEAN, 
  [HasPreviousIAM] BOOLEAN, 
  [HasAVC] BOOLEAN, 
  [HasVisualDeficit] BOOLEAN, 
  [HasAuditoryDeficit] BOOLEAN, 
  [HasDementia] BOOLEAN, 
  [HasAlcoholism] BOOLEAN, 
  [HasSmoking] BOOLEAN, 
  [HasImmunoSuppression] BOOLEAN, 
  [HasSIDA] BOOLEAN, 
  [HasRheumaticDisorder] BOOLEAN, 
  [HasPsychiatricDisorder] BOOLEAN);


CREATE TABLE [PatientEvaluation] (
  [Id] INTEGER PRIMARY KEY, 
  [PatientId] INTEGER CONSTRAINT [PatientEvaluationPatientIdFK] REFERENCES [Patient]([Id]) ON DELETE CASCADE, 
  [Date] DATE, 
  [RASS] INTEGER, 
  [DeliriumId] INTEGER, 
  [VentilationId] INTEGER, 
  [Sedation] VARCHAR, 
  [ShiftId] INTEGER, 
  [ICDSC] VARCHAR);

CREATE INDEX [PatientEvaluationPatientIdIDX] ON [PatientEvaluation] ([PatientId]);


CREATE TABLE [PatientPreDeliric] (
  [PatientId] INTEGER PRIMARY KEY REFERENCES [Patient]([Id]) ON DELETE CASCADE, 
  [IsUrgency] BOOLEAN, 
  [Morphine] INTEGER, 
  [HasInfection] BOOLEAN, 
  [Coma] INTEGER, 
  [HasSedation] BOOLEAN, 
  [Urea] FLOAT, 
  [HasAcidosis] BOOLEAN, 
  [Apache2] INTEGER, 
  [Risk] FLOAT);


