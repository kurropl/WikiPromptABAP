import React, { useState, useEffect } from 'react';
import './App.css';

const abapPrompts = {
  "seleccion-datos": {
    title: "Selección de Datos Clásica",
    icon: "🔍",
    prompts: [
      {
        title: "SELECT básico optimizado",
        description: "Prompt para generar consultas SELECT optimizadas con validaciones",
        prompt: `Genera un SELECT en ABAP para obtener datos de [TABLA] con los siguientes requisitos:
- Campos a seleccionar: [LISTA_CAMPOS]
- Condiciones WHERE: [CONDICIONES]
- Incluir validación de tabla interna no vacía
- Agregar manejo de sy-subrc
- Usar syntax moderno con @DATA y @
- Incluir comentarios explicativos
- Optimizar para performance`,
        tags: ["SELECT", "Performance", "Validación"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "JOIN de múltiples tablas",
        description: "Prompt para crear JOINs complejos con múltiples tablas",
        prompt: `Crea un JOIN en ABAP que combine las siguientes tablas:
- Tabla principal: [TABLA_PRINCIPAL] con alias [ALIAS1]
- Tablas secundarias: [TABLA2, TABLA3, etc.] con alias [ALIAS2, ALIAS3]
- Tipo de JOIN: [INNER/LEFT/RIGHT]
- Campos de unión: [CAMPOS_JOIN]
- Condiciones WHERE específicas: [CONDICIONES]
- Incluir validación de datos
- Usar sintaxis moderna
- Optimizar consulta para grandes volúmenes
- Agregar manejo de errores`,
        tags: ["JOIN", "Múltiples tablas", "Optimización"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "FOR ALL ENTRIES optimizado",
        description: "Prompt para generar FOR ALL ENTRIES con mejores prácticas",
        prompt: `Genera código ABAP usando FOR ALL ENTRIES con estas especificaciones:
- Tabla interna de entrada: [TABLA_INTERNA] 
- Tabla base de datos: [TABLA_BD]
- Campos a comparar: [CAMPOS_COMPARACION]
- Incluir validación obligatoria de tabla interna no vacía
- Implementar SORT y BINARY SEARCH para optimización
- Agregar eliminación de duplicados si es necesario
- Incluir manejo de límites de registros (máximo [NUMERO] registros)
- Usar sintaxis moderna con @DATA
- Agregar comentarios sobre mejores prácticas
- Incluir alternativa con FILTER si aplica`,
        tags: ["FOR ALL ENTRIES", "Performance", "Best Practices"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      },
      {
        title: "Corrección de consulta lenta",
        description: "Prompt para optimizar consultas existentes con problemas de performance",
        prompt: `Analiza y optimiza esta consulta ABAP que tiene problemas de performance:

[PEGAR_CONSULTA_EXISTENTE]

Aplicar las siguientes optimizaciones:
- Revisar uso de índices y sugerir mejoras
- Optimizar condiciones WHERE (orden y especificidad)
- Convertir a FOR ALL ENTRIES si usa loops anidados
- Eliminar campos innecesarios del SELECT
- Agregar SORT y BINARY SEARCH donde corresponda
- Implementar buffering si es aplicable
- Reducir accesos a base de datos
- Incluir medición de performance con GET RUN TIME
- Documentar cambios realizados
- Proporcionar versión antes/después con explicación`,
        tags: ["Optimización", "Performance Tuning", "Refactoring"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "cds-views": {
    title: "CDS Views",
    icon: "📊",
    prompts: [
      {
        title: "CDS View básica",
        description: "Prompt para crear vista CDS fundamental",
        prompt: `Crea una vista CDS básica con estas características:
- Nombre de la vista: [NOMBRE_VISTA]
- Tabla base: [TABLA_BASE]
- Campos a exponer: [LISTA_CAMPOS]
- Descripción: [DESCRIPCION]
- Incluir anotaciones estándar (@AbapCatalog, @AccessControl, @EndUserText)
- Agregar campos calculados si es necesario: [CALCULOS]
- Incluir condiciones WHERE si aplica: [FILTROS]
- Usar naming conventions para campos
- Agregar comentarios explicativos
- Seguir mejores prácticas SAP`,
        tags: ["CDS", "Vista básica", "Anotaciones"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "CDS con Associations",
        description: "Prompt para crear vistas CDS con asociaciones complejas",
        prompt: `Desarrolla una vista CDS con associations para:
- Vista principal: [NOMBRE_VISTA] basada en [TABLA_PRINCIPAL]
- Associations hacia: [TABLA_ASOCIADA1, TABLA_ASOCIADA2, etc.]
- Cardinalidad de cada association: [1..1, 1..*, 0..1, etc.]
- Condiciones de join: [CONDICIONES_JOIN]
- Campos a exponer de cada tabla
- Incluir associations transitivas si es necesario
- Implementar text associations para descripciones
- Agregar value help associations
- Optimizar performance de associations
- Usar alias descriptivos
- Documentar relaciones entre entidades`,
        tags: ["Associations", "Text associations", "Value Help"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "Analytical CDS View",
        description: "Prompt para vistas analíticas con medidas y dimensiones",
        prompt: `Crea una vista CDS analítica para reporting con:
- Tipo: @Analytics.dataCategory: #CUBE
- Tabla de hechos: [TABLA_HECHOS]
- Dimensiones principales: [LISTA_DIMENSIONES]
- Medidas a calcular: [LISTA_MEDIDAS]
- Tipo de agregación para cada medida: [SUM, AVG, COUNT, etc.]
- Jerarquías si aplica: [JERARQUIAS]
- Filtros por defecto: [FILTROS_DEFAULT]
- Incluir drill-down capabilities
- Optimizar para grandes volúmenes de datos
- Agregar currency/unit handling
- Implementar authorization checks
- Incluir time-based dimensions si es necesario
- Documentar KPIs calculados`,
        tags: ["Analytics", "KPI", "Reporting", "CUBE"],
        developmentType: "nuevo",
        complexity: "avanzado"
      },
      {
        title: "Corrección CDS con problemas",
        description: "Prompt para identificar y corregir problemas en vistas CDS",
        prompt: `Analiza y corrige esta vista CDS que presenta problemas:

[PEGAR_CDS_EXISTENTE]

Identificar y corregir:
- Errores de sintaxis o compilación
- Problemas de performance (missing indexes, complex calculations)
- Associations incorrectas o faltantes
- Anotaciones faltantes o incorrectas
- Authorization issues
- Naming conventions
- Data type inconsistencies
- Missing translations
- Currency/Unit handling
- Memory consumption issues
- Proporcionar versión corregida con explicación de cambios
- Incluir mejores prácticas aplicadas`,
        tags: ["Debugging", "Optimization", "Error Fixing"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "sap-rap": {
    title: "SAP RAP (Restful ABAP Programming)",
    icon: "🚀",
    prompts: [
      {
        title: "Behavior Definition Managed",
        description: "Prompt para crear definición de comportamiento RAP managed",
        prompt: `Crea una Behavior Definition managed para:
- Business Object: [NOMBRE_BO]
- Tabla persistente: [TABLA_PERSISTENTE]
- Tabla draft: [TABLA_DRAFT]
- Campos readonly: [CAMPOS_READONLY]
- Campos numbering managed: [CAMPOS_NUMERACION]
- Operaciones CRUD: [create, update, delete según necesidad]
- Include draft actions estándar
- Determinations necesarias: [LISTA_DETERMINATIONS]
- Validations requeridas: [LISTA_VALIDATIONS]
- Actions personalizadas si aplica: [LISTA_ACTIONS]
- Mapping de campos tabla/BO
- Incluir etag y lock management
- Agregar authorization master
- Documentar cada sección`,
        tags: ["Behavior Definition", "Managed", "CRUD", "Draft"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Service Definition y Binding",
        description: "Prompt para crear servicio OData completo",
        prompt: `Desarrolla un Service Definition y Binding para:
- Nombre del servicio: [NOMBRE_SERVICIO]
- Entities a exponer: [LISTA_ENTITIES]
- Consumption views asociadas: [VISTAS_CONSUMO]
- Tipo de binding: [OData V2/V4]
- Authentication method: [AUTHENTICATION_TYPE]
- Incluir projection views si es necesario
- Configurar authorization checks
- Implementar value helps
- Agregar text associations
- Configurar draft handling
- Incluir custom actions si aplica
- Optimizar para Fiori consumption
- Documentar endpoints disponibles
- Incluir testing recommendations`,
        tags: ["Service Definition", "OData", "Fiori", "API"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Metadata Extensions para Fiori",
        description: "Prompt para crear anotaciones UI completas",
        prompt: `Crea Metadata Extensions para vista [NOMBRE_VISTA] con:
- Header information configurado
- Facets structure: [ESTRUCTURA_FACETS]
- Line items principales: [CAMPOS_LINEITEM]
- Identification fields: [CAMPOS_IDENTIFICATION]
- Field groups organization: [GRUPOS_CAMPOS]
- Search helps configuration: [VALUE_HELPS]
- Field criticality si aplica: [CAMPOS_CRITICALITY]
- Actions en toolbar: [ACCIONES_TOOLBAR]
- Responsive design considerations
- Multi-language support
- Field validation annotations
- Quick filters setup
- Export capabilities
- Print functionality
- Navigation properties
- Incluir best practices para UX`,
        tags: ["Metadata Extensions", "Fiori UI", "Annotations", "UX"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "Implementation Class RAP",
        description: "Prompt para clase de implementación con lógica de negocio",
        prompt: `Genera Implementation Class para Behavior Definition con:
- Nombre de la clase: [NOMBRE_CLASE]
- Entity a implementar: [ENTITY_NAME]
- Determinations a implementar: [LISTA_DETERMINATIONS]
- Validations a implementar: [LISTA_VALIDATIONS]
- Actions a implementar: [LISTA_ACTIONS]
- Lógica de negocio específica: [DESCRIPCION_LOGICA]
- Error handling robusto
- Message handling con severity levels
- Side effects implementation
- Performance optimization
- Unit tests structure
- Authorization checks
- Logging implementation
- Transaction handling
- Include exception scenarios
- Documentar cada método con examples`,
        tags: ["Implementation Class", "Business Logic", "Error Handling"],
        developmentType: "nuevo",
        complexity: "avanzado"
      }
    ]
  },
  "odata-services": {
    title: "OData Services",
    icon: "🌐",
    prompts: [
      {
        title: "OData Service V4 básico",
        description: "Prompt para crear servicio OData V4 desde cero",
        prompt: `Crea un OData Service V4 completo para:
- Nombre del servicio: [NOMBRE_SERVICIO]
- Entidades principales: [LISTA_ENTIDADES]
- Operaciones CRUD requeridas: [CREATE, READ, UPDATE, DELETE]
- Filtros necesarios: [LISTA_FILTROS]
- Ordenamiento por defecto: [CAMPOS_ORDEN]
- Paginación configurada
- Incluir $expand para navegaciones
- Implementar $count functionality
- Error handling estándar OData
- Authentication y authorization
- Cross-origin resource sharing (CORS)
- API documentation annotations
- Performance optimization
- Caching strategies
- Rate limiting si es necesario
- Testing endpoints examples`,
        tags: ["OData V4", "REST API", "CRUD", "Performance"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Custom Actions en OData",
        description: "Prompt para implementar acciones personalizadas",
        prompt: `Implementa Custom Actions en OData service para:
- Servicio base: [NOMBRE_SERVICIO]
- Actions a crear: [LISTA_ACTIONS]
- Parámetros de entrada para cada action: [PARAMETROS_ENTRADA]
- Valores de retorno: [TIPOS_RETORNO]
- Validaciones de entrada requeridas
- Business logic a implementar: [DESCRIPCION_LOGICA]
- Error scenarios y messages
- Authorization checks específicos
- Transactional behavior
- Idempotency considerations
- Audit logging
- Performance monitoring
- Integration with workflow si aplica
- Testing scenarios
- Documentation para consumers
- Versioning strategy`,
        tags: ["Custom Actions", "Business Logic", "Transaction", "Security"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "OData con Deep Insert",
        description: "Prompt para operaciones complejas con entidades relacionadas",
        prompt: `Implementa Deep Insert functionality para:
- Entidad principal: [ENTIDAD_PRINCIPAL]
- Entidades relacionadas: [ENTIDADES_RELACIONADAS]
- Relaciones entre entidades: [TIPO_RELACIONES]
- Validaciones cross-entity
- Transaction scope management
- Rollback scenarios
- Batch processing support
- Error handling granular
- Optimistic locking
- Authorization checks por entity
- Audit trail implementation
- Performance optimization para bulk operations
- Memory management
- Progress tracking
- Recovery mechanisms
- Testing complex scenarios
- Integration patterns`,
        tags: ["Deep Insert", "Batch", "Transaction", "Complex Operations"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      }
    ]
  },
  "workflow": {
    title: "Workflow y Procesos",
    icon: "🔄",
    prompts: [
      {
        title: "Business Workflow básico",
        description: "Prompt para crear workflow de aprobación estándar",
        prompt: `Crea un Business Workflow para proceso de aprobación con:
- Nombre del workflow: [NOMBRE_WORKFLOW]
- Objeto de negocio: [BUSINESS_OBJECT]
- Pasos del proceso: [LISTA_PASOS]
- Roles de aprobación: [ROLES_USUARIOS]
- Criterios de escalación: [CRITERIOS_ESCALACION]
- Timeouts y deadlines: [TIEMPOS_LIMITE]
- Notificaciones por email/portal
- Decision points y routing logic
- Parallel processing si es necesario
- Exception handling
- Audit trail completo
- Status tracking
- Restart capability
- Integration con SAP Inbox
- Mobile support considerations
- Reporting de performance
- Testing scenarios`,
        tags: ["Workflow", "Approval", "Business Process", "Automation"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Event-based Workflow",
        description: "Prompt para workflow automático basado en eventos",
        prompt: `Implementa Event-based Workflow para:
- Evento disparador: [TIPO_EVENTO]
- Condiciones de activación: [CONDICIONES]
- Objeto de negocio afectado: [BUSINESS_OBJECT]
- Acciones automáticas a ejecutar: [LISTA_ACCIONES]
- Reglas de negocio: [REGLAS_NEGOCIO]
- Integration points: [SISTEMAS_INTEGRADOS]
- Error recovery mechanisms
- Retry logic con backoff strategy
- Monitoring y alerting
- Performance optimization
- Scalability considerations
- Security y authorization
- Logging detallado
- Configuration management
- Testing automation
- Documentation completa
- Maintenance procedures`,
        tags: ["Event-driven", "Automation", "Integration", "Monitoring"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "Workflow Error Recovery",
        description: "Prompt para manejo robusto de errores en workflows",
        prompt: `Desarrolla sistema de Error Recovery para workflows con:
- Tipos de errores a manejar: [TIPOS_ERRORES]
- Estrategias de retry: [ESTRATEGIAS_RETRY]
- Escalation procedures: [PROCEDIMIENTOS_ESCALACION]
- Rollback mechanisms: [MECANISMOS_ROLLBACK]
- Notification systems: [SISTEMAS_NOTIFICACION]
- Error categorization y priority
- Automatic vs manual recovery
- Data consistency checks
- Compensation actions
- Dead letter queue handling
- Monitoring dashboards
- Root cause analysis tools
- Performance impact analysis
- Documentation de troubleshooting
- Prevention mechanisms
- Testing error scenarios
- Recovery time objectives`,
        tags: ["Error Handling", "Recovery", "Monitoring", "Resilience"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "amdp": {
    title: "AMDP (ABAP Managed Database Procedures)",
    icon: "🗄️",
    prompts: [
      {
        title: "AMDP básico para optimización",
        description: "Prompt para crear procedimiento AMDP simple",
        prompt: `Crea un AMDP básico para optimizar consulta con:
- Nombre de la clase: [NOMBRE_CLASE_AMDP]
- Método principal: [NOMBRE_METODO]
- Tablas de entrada: [TABLAS_INPUT]
- Parámetros de entrada: [PARAMETROS_ENTRADA]
- Resultado esperado: [ESTRUCTURA_RESULTADO]
- Lógica SQL específica: [DESCRIPCION_LOGICA]
- Optimizaciones HANA específicas
- Error handling en SQLScript
- Performance monitoring
- Memory optimization
- Parallel processing hints
- Index usage optimization
- Comentarios explicativos
- Testing approach
- Comparison con ABAP equivalente
- Maintenance considerations`,
        tags: ["AMDP", "SQLScript", "Performance", "HANA"],
        developmentType: "optimizacion",
        complexity: "intermedio"
      },
      {
        title: "AMDP Table Function",
        description: "Prompt para función AMDP compleja con cálculos",
        prompt: `Desarrolla AMDP Table Function para cálculos complejos:
- Nombre de la función: [NOMBRE_FUNCION]
- Parámetros de entrada: [PARAMETROS]
- Tablas base involucradas: [TABLAS_BASE]
- Cálculos a realizar: [DESCRIPCION_CALCULOS]
- Agregaciones necesarias: [AGREGACIONES]
- Window functions requeridas: [WINDOW_FUNCTIONS]
- Filtros dinámicos: [FILTROS_DINAMICOS]
- Performance requirements: [REQUISITOS_PERFORMANCE]
- Memory constraints: [LIMITACIONES_MEMORIA]
- Parallel execution strategy
- Error scenarios handling
- Result set optimization
- Caching opportunities
- Integration con CDS Views
- Testing complex scenarios
- Documentation completa`,
        tags: ["Table Function", "Complex Calculations", "Window Functions"],
        developmentType: "nuevo",
        complexity: "avanzado"
      },
      {
        title: "AMDP con Error Handling",
        description: "Prompt para AMDP robusto con manejo de errores",
        prompt: `Implementa AMDP con manejo robusto de errores para:
- Procedimiento base: [NOMBRE_PROCEDIMIENTO]
- Escenarios de error esperados: [ESCENARIOS_ERROR]
- Validaciones de entrada: [VALIDACIONES]
- Rollback procedures: [PROCEDIMIENTOS_ROLLBACK]
- Logging mechanisms: [MECANISMOS_LOG]
- Exception handling patterns
- Data consistency checks
- Performance degradation detection
- Memory overflow protection
- Timeout handling
- Resource cleanup procedures
- Error notification systems
- Recovery mechanisms
- Diagnostic information collection
- Testing error scenarios
- Monitoring integration
- Documentation de troubleshooting`,
        tags: ["Error Handling", "Robustness", "Monitoring", "Diagnostics"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "alv-reports": {
    title: "Reportes ALV",
    icon: "📋",
    prompts: [
      {
        title: "ALV Grid moderno",
        description: "Prompt para reporte ALV con funcionalidades completas",
        prompt: `Crea un reporte ALV moderno con:
- Nombre del programa: [NOMBRE_PROGRAMA]
- Datos a mostrar: [DESCRIPCION_DATOS]
- Tabla/vista fuente: [FUENTE_DATOS]
- Campos en el ALV: [LISTA_CAMPOS]
- Filtros de selección: [PANTALLA_SELECCION]
- Funcionalidades requeridas: [FUNCIONALIDADES]
- Botones personalizados: [BOTONES_CUSTOM]
- Validaciones de entrada
- Export functionality (Excel, PDF)
- Print capabilities
- Column optimization
- Sum/subtotal functionality
- Color coding por condiciones
- Hotspot functionality
- Navigation capabilities
- Performance optimization
- User settings persistence
- Authorization checks
- Error handling completo`,
        tags: ["ALV Grid", "Report", "Export", "User Interface"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "ALV con jerarquías",
        description: "Prompt para ALV Tree con estructura jerárquica",
        prompt: `Desarrolla ALV Tree report con estructura jerárquica:
- Estructura de jerarquía: [DESCRIPCION_JERARQUIA]
- Niveles de agrupación: [NIVELES_GRUPO]
- Datos padre: [ENTIDAD_PADRE]
- Datos hijo: [ENTIDADES_HIJO]
- Agregaciones por nivel: [AGREGACIONES]
- Expand/collapse functionality
- Drill-down capabilities
- Context menus por nivel
- Different icons por tipo de nodo
- Sorting multi-level
- Filtering jerárquico
- Export manteniendo estructura
- Performance optimization para grandes volúmenes
- Memory management
- User interaction patterns
- Mobile responsive considerations
- Accessibility features`,
        tags: ["ALV Tree", "Hierarchy", "Drill-down", "Complex Structure"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "ALV con validación de datos",
        description: "Prompt para ALV con capacidades de edición y validación",
        prompt: `Crea ALV editable con validación de datos para:
- Entidad a editar: [ENTIDAD_PRINCIPAL]
- Campos editables: [CAMPOS_EDITABLES]
- Campos readonly: [CAMPOS_READONLY]
- Validaciones por campo: [REGLAS_VALIDACION]
- Validaciones cross-field: [VALIDACIONES_CRUZADAS]
- Save functionality con transacciones
- Rollback capabilities
- Change tracking
- Mass operations (copy, delete, modify)
- Data quality checks
- Duplicate detection
- Authorization por campo
- Audit trail
- Error highlighting
- Batch processing
- Conflict resolution
- Integration con change documents
- Testing scenarios`,
        tags: ["Editable ALV", "Data Validation", "Transaction", "Quality"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "forms": {
    title: "Formularios",
    icon: "📄",
    prompts: [
      {
        title: "SmartForm básico",
        description: "Prompt para crear SmartForm estándar",
        prompt: `Crea un SmartForm para [TIPO_DOCUMENTO] con:
- Nombre del formulario: [NOMBRE_FORM]
- Datos de entrada: [ESTRUCTURA_DATOS]
- Secciones del documento: [SECCIONES]
- Header information: [INFO_HEADER]
- Line items structure: [ESTRUCTURA_ITEMS]
- Footer information: [INFO_FOOTER]
- Logos y imágenes: [ELEMENTOS_GRAFICOS]
- Conditional texts: [TEXTOS_CONDICIONALES]
- Calculations: [CALCULOS]
- Multiple language support
- Print parameters configuration
- Email integration
- PDF generation settings
- Archive integration
- Layout responsive design
- Testing procedures
- Maintenance documentation`,
        tags: ["SmartForm", "Document", "Print", "PDF"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "Adobe Forms interactivo",
        description: "Prompt para formulario Adobe con interactividad",
        prompt: `Desarrolla Adobe Form interactivo para:
- Propósito del formulario: [PROPOSITO]
- Campos de entrada interactivos: [CAMPOS_ENTRADA]
- Validaciones client-side: [VALIDACIONES_CLIENT]
- Cálculos automáticos: [CALCULOS_AUTO]
- Conditional logic: [LOGICA_CONDICIONAL]
- Submit functionality: [FUNCIONALIDAD_SUBMIT]
- Integration con backend: [INTEGRACION_BACKEND]
- Digital signature support
- Form versioning
- User role-based fields
- Save/resume capability
- Mobile responsiveness
- Accessibility compliance
- Multi-language support
- Print optimization
- Security considerations
- Testing scenarios
- Distribution mechanisms`,
        tags: ["Adobe Forms", "Interactive", "Digital Signature", "Mobile"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "Corrección formularios con errores",
        description: "Prompt para diagnosticar y corregir problemas en formularios",
        prompt: `Analiza y corrige problemas en formulario existente:

[DESCRIPCION_PROBLEMA_ACTUAL]

Diagnosticar y solucionar:
- Errores de layout y formatting
- Performance issues en generación
- Memory problems con grandes volúmenes
- Character encoding issues
- Print quality problems
- Missing data o campos vacíos
- Integration failures
- Version compatibility issues
- Authorization problems
- Archive/email delivery failures
- Mobile rendering problems
- Multi-language issues
- Digital signature errors
- Proporcionar solución paso a paso
- Incluir testing plan
- Preventive measures`,
        tags: ["Troubleshooting", "Error Resolution", "Performance", "Quality"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "bapis": {
    title: "BAPIs y Function Modules",
    icon: "🔧",
    prompts: [
      {
        title: "BAPI estándar optimizado",
        description: "Prompt para uso eficiente de BAPIs estándar",
        prompt: `Implementa llamada optimizada a BAPI estándar para:
- BAPI a utilizar: [NOMBRE_BAPI]
- Operación objetivo: [OPERACION]
- Datos de entrada: [ESTRUCTURA_ENTRADA]
- Validaciones previas: [VALIDACIONES_PREVIAS]
- Error handling robusto
- Commit/rollback logic
- Batch processing si aplica
- Performance optimization
- Authorization checks
- Logging implementation
- Retry mechanism
- Transaction scope management
- Memory management
- Progress tracking
- Integration testing
- Documentation completa
- Best practices adherence
- Monitoring integration`,
        tags: ["BAPI", "Error Handling", "Performance", "Best Practices"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Function Module personalizado",
        description: "Prompt para crear función reutilizable",
        prompt: `Desarrolla Function Module personalizado para:
- Nombre de la función: [NOMBRE_FUNCION]
- Propósito específico: [PROPOSITO]
- Parámetros de entrada: [PARAMETROS_ENTRADA]
- Parámetros de salida: [PARAMETROS_SALIDA]
- Tablas internas: [TABLAS_INTERNAS]
- Lógica de negocio: [DESCRIPCION_LOGICA]
- Exception handling
- Input validation
- Performance optimization
- Memory efficient processing
- Transactional behavior
- Authorization integration
- Logging capabilities
- Documentation estándar SAP
- Testing framework
- Version management
- Backward compatibility
- Integration guidelines`,
        tags: ["Function Module", "Reusable", "Business Logic", "Documentation"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "BAPI con manejo de errores avanzado",
        description: "Prompt para implementación robusta con recuperación de errores",
        prompt: `Implementa BAPI call con error handling avanzado para:
- BAPI principal: [NOMBRE_BAPI]
- Escenarios de error esperados: [ESCENARIOS_ERROR]
- Estrategias de recovery: [ESTRATEGIAS_RECOVERY]
- Compensation logic: [LOGICA_COMPENSACION]
- Retry patterns con exponential backoff
- Circuit breaker implementation
- Dead letter queue handling
- Correlation ID tracking
- Distributed transaction support
- Saga pattern si es necesario
- Monitoring y alerting
- Audit trail completo
- Performance impact analysis
- Chaos engineering considerations
- Documentation de runbooks
- Incident response procedures
- Recovery time objectives
- Testing fault scenarios`,
        tags: ["Advanced Error Handling", "Resilience", "Distributed Systems"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "modifications": {
    title: "Modificaciones",
    icon: "⚙️",
    prompts: [
      {
        title: "User-Exit implementation",
        description: "Prompt para implementar User-Exit estándar",
        prompt: `Implementa User-Exit para:
- Programa/transacción: [PROGRAMA_BASE]
- User-Exit específico: [NOMBRE_EXIT]
- Trigger point: [PUNTO_ACTIVACION]
- Lógica de negocio requerida: [LOGICA_NEGOCIO]
- Validaciones a implementar: [VALIDACIONES]
- Modificaciones de datos: [MODIFICACIONES]
- Error handling
- Performance considerations
- Impact analysis
- Testing approach
- Documentation completa
- Upgrade compatibility
- Rollback procedures
- Authorization checks
- Audit trail
- Change management
- Best practices compliance
- Alternative solutions analysis`,
        tags: ["User-Exit", "Standard Enhancement", "Business Logic"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "BAdI Implementation",
        description: "Prompt para implementar Business Add-In",
        prompt: `Desarrolla BAdI Implementation para:
- BAdI definition: [NOMBRE_BADI]
- Implementation class: [NOMBRE_CLASE]
- Interface methods: [METODOS_INTERFACE]
- Filter criteria si aplica: [CRITERIOS_FILTRO]
- Business logic específica: [LOGICA_ESPECIFICA]
- Context data handling: [MANEJO_CONTEXTO]
- Multi-implementation support
- Priority handling
- Error propagation
- Performance optimization
- Authorization integration
- Configuration management
- Testing strategies
- Documentation estándar
- Upgrade considerations
- Monitoring capabilities
- Change impact analysis
- Deployment procedures`,
        tags: ["BAdI", "Implementation Class", "Interface", "Configuration"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "Enhancement Spot personalizado",
        description: "Prompt para crear Enhancement Spot desde cero",
        prompt: `Crea Enhancement Spot personalizado para:
- Nombre del Enhancement Spot: [NOMBRE_SPOT]
- Localización en código: [UBICACION_CODIGO]
- Tipos de enhancement soportados: [TIPOS_ENHANCEMENT]
- Interface definition: [DEFINICION_INTERFACE]
- Parameters disponibles: [PARAMETROS_DISPONIBLES]
- Implementation guidelines: [GUIAS_IMPLEMENTACION]
- Security considerations
- Performance impact assessment
- Versioning strategy
- Documentation templates
- Testing framework
- Implementation examples
- Best practices guide
- Troubleshooting procedures
- Migration path planning
- Community guidelines
- Support procedures
- Evolution roadmap`,
        tags: ["Enhancement Spot", "Framework", "Extensibility", "Architecture"],
        developmentType: "nuevo",
        complexity: "avanzado"
      }
    ]
  },
  "performance": {
    title: "Performance y Optimización",
    icon: "⚡",
    prompts: [
      {
        title: "Análisis de performance básico",
        description: "Prompt para identificar y resolver problemas de performance",
        prompt: `Analiza y optimiza performance del siguiente código ABAP:

[PEGAR_CODIGO_A_OPTIMIZAR]

Realizar análisis de:
- Database access patterns
- Loop optimization opportunities
- Memory usage patterns
- CPU intensive operations
- Network round trips
- Caching opportunities
- Index usage verification
- Parallel processing possibilities
- Buffer utilization
- Transaction scope optimization
- Progress indicator implementation
- Resource cleanup
- Bottleneck identification
- Before/after comparison
- Performance measurement
- Monitoring recommendations
- Preventive measures`,
        tags: ["Performance Analysis", "Optimization", "Bottleneck", "Monitoring"],
        developmentType: "optimizacion",
        complexity: "intermedio"
      },
      {
        title: "Optimización avanzada con paralelización",
        description: "Prompt para implementar procesamiento paralelo",
        prompt: `Implementa optimización avanzada con paralelización para:
- Proceso actual: [DESCRIPCION_PROCESO]
- Volumen de datos: [VOLUMEN_DATOS]
- Time constraints: [RESTRICCIONES_TIEMPO]
- Available resources: [RECURSOS_DISPONIBLES]
- Parallel processing strategy: [ESTRATEGIA_PARALELA]
- Work distribution algorithm
- Synchronization mechanisms
- Error handling en contexto paralelo
- Resource contention management
- Load balancing
- Progress monitoring agregado
- Memory management distribuido
- Transaction coordination
- Fault tolerance
- Scalability considerations
- Performance metrics collection
- Testing parallel scenarios
- Deployment strategy`,
        tags: ["Parallel Processing", "Advanced Optimization", "Scalability"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      },
      {
        title: "Memory management optimizado",
        description: "Prompt para gestión eficiente de memoria",
        prompt: `Optimiza memory management para aplicación con:
- Current memory issues: [PROBLEMAS_MEMORIA_ACTUALES]
- Data volume characteristics: [CARACTERISTICAS_VOLUMEN]
- Processing patterns: [PATRONES_PROCESAMIENTO]
- Memory constraints: [LIMITACIONES_MEMORIA]
- Garbage collection strategy
- Object lifecycle management
- Cache optimization
- Stream processing implementation
- Chunk processing algorithms
- Memory leak prevention
- Resource pooling
- Lazy loading patterns
- Memory profiling integration
- Monitoring y alerting
- Capacity planning
- Performance testing
- Documentation de patterns
- Best practices guide`,
        tags: ["Memory Management", "Resource Optimization", "Capacity Planning"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      }
    ]
  },
  "debugging": {
    title: "Debugging y Testing",
    icon: "🐛",
    prompts: [
      {
        title: "Debugging strategy completa",
        description: "Prompt para implementar debugging comprehensivo",
        prompt: `Desarrolla debugging strategy para:
- Aplicación/módulo: [NOMBRE_APLICACION]
- Problemas reportados: [DESCRIPCION_PROBLEMAS]
- Environment details: [DETALLES_AMBIENTE]
- Reproduction steps: [PASOS_REPRODUCCION]
- Debug tools a utilizar: [HERRAMIENTAS_DEBUG]
- Logging strategy implementation
- Breakpoint strategy
- Variable inspection approach
- Performance profiling
- Memory analysis
- Call stack analysis
- Exception handling verification
- Data flow tracing
- Integration point testing
- Root cause analysis methodology
- Fix verification approach
- Regression testing plan
- Documentation de findings`,
        tags: ["Debugging Strategy", "Root Cause Analysis", "Testing"],
        developmentType: "correctivo",
        complexity: "intermedio"
      },
      {
        title: "Unit Testing framework",
        description: "Prompt para crear framework de testing robusto",
        prompt: `Crea Unit Testing framework completo para:
- Módulo a testear: [MODULO_OBJETIVO]
- Coverage requirements: [REQUISITOS_COBERTURA]
- Test scenarios: [ESCENARIOS_TEST]
- Mock objects needed: [OBJETOS_MOCK]
- Test data management: [GESTION_DATOS_TEST]
- Assertion strategies
- Test fixture setup
- Teardown procedures
- Parameterized testing
- Performance testing integration
- Integration testing approach
- Continuous testing pipeline
- Test reporting
- Coverage analysis
- Mutation testing
- Property-based testing
- Contract testing
- Test maintenance procedures
- Documentation de tests`,
        tags: ["Unit Testing", "Test Framework", "Coverage", "Automation"],
        developmentType: "nuevo",
        complexity: "avanzado"
      },
      {
        title: "Production debugging avanzado",
        description: "Prompt para debugging en ambiente productivo",
        prompt: `Implementa production debugging capabilities para:
- Production issues: [ISSUES_PRODUCCION]
- Business impact: [IMPACTO_NEGOCIO]
- Available debugging windows: [VENTANAS_DEBUG]
- Risk mitigation measures: [MEDIDAS_MITIGACION]
- Non-intrusive debugging techniques
- Log analysis automation
- Performance monitoring integration
- Real-time diagnostics
- Remote debugging setup
- Canary deployment testing
- Feature flag implementation
- Circuit breaker monitoring
- Health check systems
- Incident response procedures
- Rollback strategies
- Communication protocols
- Post-incident analysis
- Prevention measures implementation`,
        tags: ["Production Debugging", "Incident Response", "Monitoring"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  }
};

const developmentTypes = {
  "nuevo": { label: "Nuevo", color: "bg-green-100 text-green-800", icon: "🆕" },
  "correctivo": { label: "Correctivo", color: "bg-red-100 text-red-800", icon: "🔧" },
  "evolutivo": { label: "Evolutivo", color: "bg-blue-100 text-blue-800", icon: "📈" },
  "optimizacion": { label: "Optimización", color: "bg-purple-100 text-purple-800", icon: "⚡" }
};

const complexityLevels = {
  "básico": { label: "Básico", color: "bg-gray-100 text-gray-800" },
  "intermedio": { label: "Intermedio", color: "bg-yellow-100 text-yellow-800" },
  "avanzado": { label: "Avanzado", color: "bg-orange-100 text-orange-800" }
};

function App() {
  const [selectedCategory, setSelectedCategory] = useState('seleccion-datos');
  const [searchTerm, setSearchTerm] = useState('');
  const [selectedDevType, setSelectedDevType] = useState('');
  const [selectedComplexity, setSelectedComplexity] = useState('');
  const [copiedCode, setCopiedCode] = useState('');

  const copyToClipboard = (prompt, title) => {
    navigator.clipboard.writeText(prompt);
    setCopiedCode(title);
    setTimeout(() => setCopiedCode(''), 2000);
  };

  // Filtrar prompts basado en criterios
  const filteredPrompts = Object.entries(abapPrompts).reduce((acc, [key, category]) => {
    const filtered = category.prompts.filter(prompt => {
      const matchesSearch = searchTerm === '' || 
        prompt.title.toLowerCase().includes(searchTerm.toLowerCase()) ||
        prompt.description.toLowerCase().includes(searchTerm.toLowerCase()) ||
        prompt.tags.some(tag => tag.toLowerCase().includes(searchTerm.toLowerCase()));
      
      const matchesDevType = selectedDevType === '' || prompt.developmentType === selectedDevType;
      const matchesComplexity = selectedComplexity === '' || prompt.complexity === selectedComplexity;
      
      return matchesSearch && matchesDevType && matchesComplexity;
    });
    
    if (filtered.length > 0) {
      acc[key] = { ...category, prompts: filtered };
    }
    return acc;
  }, {});

  const totalPrompts = Object.values(abapPrompts).reduce((sum, category) => sum + category.prompts.length, 0);
  const filteredCount = Object.values(filteredPrompts).reduce((sum, category) => sum + category.prompts.length, 0);

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100">
      {/* Header */}
      <header className="bg-white shadow-lg border-b-4 border-blue-600">
        <div className="container mx-auto px-6 py-6">
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-4">
              <div className="w-12 h-12 bg-blue-600 rounded-lg flex items-center justify-center">
                <span className="text-white text-2xl font-bold">P</span>
              </div>
              <div>
                <h1 className="text-3xl font-bold text-gray-800">ABAP Prompts Wiki</h1>
                <p className="text-gray-600">Los mejores prompts optimizados para Copilot 365 y desarrollo ABAP - {totalPrompts} prompts disponibles</p>
              </div>
            </div>
          </div>
        </div>
      </header>

      <div className="container mx-auto px-6 py-8">
        {/* Info Banner */}
        <div className="bg-gradient-to-r from-blue-600 to-purple-600 text-white rounded-xl p-6 mb-8">
          <div className="flex items-center space-x-3 mb-2">
            <span className="text-2xl">🤖</span>
            <h2 className="text-xl font-bold">Prompts optimizados para Copilot 365</h2>
          </div>
          <p className="text-blue-100">
            Cada prompt está diseñado para copiar y pegar directamente en Copilot 365, GitHub Copilot, o cualquier IA. 
            Solo reemplaza los valores entre [CORCHETES] con tus datos específicos.
          </p>
        </div>

        <div className="flex flex-col lg:flex-row gap-8">
          {/* Sidebar */}
          <div className="lg:w-1/4">
            <div className="bg-white rounded-xl shadow-lg p-6 sticky top-8 space-y-6">
              {/* Filtros por tipo de desarrollo */}
              <div>
                <h3 className="text-lg font-bold text-gray-800 mb-3">Tipo de Desarrollo</h3>
                <div className="space-y-2">
                  <button
                    onClick={() => setSelectedDevType('')}
                    className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 flex items-center space-x-2 ${
                      selectedDevType === '' ? 'bg-gray-600 text-white' : 'text-gray-700 hover:bg-gray-50'
                    }`}
                  >
                    <span>📋</span>
                    <span>Todos</span>
                  </button>
                  {Object.entries(developmentTypes).map(([key, type]) => (
                    <button
                      key={key}
                      onClick={() => setSelectedDevType(key)}
                      className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 flex items-center space-x-2 ${
                        selectedDevType === key ? 'bg-gray-600 text-white' : 'text-gray-700 hover:bg-gray-50'
                      }`}
                    >
                      <span>{type.icon}</span>
                      <span>{type.label}</span>
                    </button>
                  ))}
                </div>
              </div>

              {/* Filtros por complejidad */}
              <div>
                <h3 className="text-lg font-bold text-gray-800 mb-3">Complejidad</h3>
                <div className="space-y-2">
                  <button
                    onClick={() => setSelectedComplexity('')}
                    className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 ${
                      selectedComplexity === '' ? 'bg-gray-600 text-white' : 'text-gray-700 hover:bg-gray-50'
                    }`}
                  >
                    Todas
                  </button>
                  {Object.entries(complexityLevels).map(([key, level]) => (
                    <button
                      key={key}
                      onClick={() => setSelectedComplexity(key)}
                      className={`w-full text-left px-3 py-2 rounded-lg transition-all duration-200 ${
                        selectedComplexity === key ? 'bg-gray-600 text-white' : 'text-gray-700 hover:bg-gray-50'
                      }`}
                    >
                      {level.label}
                    </button>
                  ))}
                </div>
              </div>

              {/* Categorías */}
              <div>
                <h2 className="text-xl font-bold text-gray-800 mb-4">Categorías</h2>
                <nav className="space-y-2">
                  {Object.entries(abapPrompts).map(([key, category]) => (
                    <button
                      key={key}
                      onClick={() => setSelectedCategory(key)}
                      className={`w-full text-left px-4 py-3 rounded-lg transition-all duration-200 flex items-center space-x-3 ${
                        selectedCategory === key
                          ? 'bg-blue-600 text-white shadow-lg'
                          : 'text-gray-700 hover:bg-blue-50'
                      }`}
                    >
                      <span className="text-lg">{category.icon}</span>
                      <span className="font-medium">{category.title}</span>
                    </button>
                  ))}
                </nav>
              </div>
            </div>
          </div>

          {/* Main Content */}
          <div className="lg:w-3/4">
            {/* Search Bar y filtros activos */}
            <div className="bg-white rounded-xl shadow-lg p-6 mb-8">
              <div className="relative mb-4">
                <input
                  type="text"
                  placeholder="Buscar prompts por nombre, descripción o tags..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  className="w-full px-4 py-3 pl-12 border-2 border-gray-200 rounded-lg focus:border-blue-500 focus:outline-none transition-colors"
                />
                <div className="absolute left-4 top-3.5 text-gray-400">
                  <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                  </svg>
                </div>
              </div>
              
              {/* Filtros activos */}
              <div className="flex flex-wrap gap-2">
                {selectedDevType && (
                  <span className={`px-3 py-1 rounded-full text-sm font-medium flex items-center space-x-1 ${developmentTypes[selectedDevType].color}`}>
                    <span>{developmentTypes[selectedDevType].icon}</span>
                    <span>{developmentTypes[selectedDevType].label}</span>
                    <button onClick={() => setSelectedDevType('')} className="ml-1 hover:bg-black hover:bg-opacity-10 rounded-full p-0.5">×</button>
                  </span>
                )}
                {selectedComplexity && (
                  <span className={`px-3 py-1 rounded-full text-sm font-medium flex items-center space-x-1 ${complexityLevels[selectedComplexity].color}`}>
                    <span>{complexityLevels[selectedComplexity].label}</span>
                    <button onClick={() => setSelectedComplexity('')} className="ml-1 hover:bg-black hover:bg-opacity-10 rounded-full p-0.5">×</button>
                  </span>
                )}
              </div>
              
              <p className="mt-2 text-sm text-gray-600">
                Mostrando {filteredCount} de {totalPrompts} prompts
                {searchTerm && ` para "${searchTerm}"`}
              </p>
            </div>

            {/* Current Category Header */}
            {!searchTerm && !selectedDevType && !selectedComplexity && (
              <div className="bg-white rounded-xl shadow-lg p-6 mb-8">
                <div className="flex items-center space-x-4">
                  <span className="text-4xl">{abapPrompts[selectedCategory].icon}</span>
                  <div>
                    <h2 className="text-2xl font-bold text-gray-800">{abapPrompts[selectedCategory].title}</h2>
                    <p className="text-gray-600">{abapPrompts[selectedCategory].prompts.length} prompts disponibles</p>
                  </div>
                </div>
              </div>
            )}

            {/* Prompts Grid */}
            <div className="space-y-6">
              {(searchTerm || selectedDevType || selectedComplexity ? Object.entries(filteredPrompts) : [[selectedCategory, abapPrompts[selectedCategory]]]).map(([categoryKey, category]) => (
                <div key={categoryKey}>
                  {(searchTerm || selectedDevType || selectedComplexity) && (
                    <h3 className="text-xl font-bold text-gray-800 mb-4 flex items-center space-x-2">
                      <span>{category.icon}</span>
                      <span>{category.title}</span>
                    </h3>
                  )}
                  {category.prompts.map((prompt, index) => (
                    <div key={index} className="bg-white rounded-xl shadow-lg overflow-hidden hover:shadow-xl transition-shadow duration-300">
                      <div className="p-6">
                        <div className="flex justify-between items-start mb-4">
                          <div className="flex-1">
                            <h3 className="text-xl font-bold text-gray-800 mb-2">{prompt.title}</h3>
                            <p className="text-gray-600 mb-3">{prompt.description}</p>
                            
                            {/* Tags y metadatos */}
                            <div className="flex flex-wrap gap-2 mb-3">
                              <span className={`px-3 py-1 rounded-full text-sm font-medium flex items-center space-x-1 ${developmentTypes[prompt.developmentType]?.color || 'bg-gray-100 text-gray-800'}`}>
                                <span>{developmentTypes[prompt.developmentType]?.icon}</span>
                                <span>{developmentTypes[prompt.developmentType]?.label}</span>
                              </span>
                              <span className={`px-3 py-1 rounded-full text-sm font-medium ${complexityLevels[prompt.complexity]?.color || 'bg-gray-100 text-gray-800'}`}>
                                {complexityLevels[prompt.complexity]?.label}
                              </span>
                              {prompt.tags.map((tag, tagIndex) => (
                                <span
                                  key={tagIndex}
                                  className="px-3 py-1 bg-blue-100 text-blue-800 rounded-full text-sm font-medium"
                                >
                                  {tag}
                                </span>
                              ))}
                            </div>
                          </div>
                          <button
                            onClick={() => copyToClipboard(prompt.prompt, prompt.title)}
                            className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg transition-colors duration-200 flex items-center space-x-2 ml-4"
                          >
                            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                            </svg>
                            <span>{copiedCode === prompt.title ? '¡Copiado!' : 'Copiar Prompt'}</span>
                          </button>
                        </div>
                        
                        <div className="bg-gray-900 rounded-lg overflow-hidden">
                          <div className="bg-gray-800 px-4 py-2 flex items-center justify-between">
                            <span className="text-gray-300 text-sm font-medium flex items-center space-x-2">
                              <span>🤖</span>
                              <span>Prompt para Copilot 365</span>
                            </span>
                            <div className="flex space-x-2">
                              <div className="w-3 h-3 bg-red-500 rounded-full"></div>
                              <div className="w-3 h-3 bg-yellow-500 rounded-full"></div>
                              <div className="w-3 h-3 bg-green-500 rounded-full"></div>
                            </div>
                          </div>
                          <pre className="p-4 text-sm text-green-400 font-mono overflow-x-auto whitespace-pre-wrap">
                            <code>{prompt.prompt}</code>
                          </pre>
                        </div>
                        
                        <div className="mt-4 p-3 bg-blue-50 rounded-lg">
                          <p className="text-sm text-blue-800">
                            <span className="font-semibold">💡 Tip:</span> Reemplaza los valores entre [CORCHETES] con tus datos específicos antes de usar en Copilot 365.
                          </p>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              ))}
            </div>

            {/* No Results */}
            {(searchTerm || selectedDevType || selectedComplexity) && Object.keys(filteredPrompts).length === 0 && (
              <div className="bg-white rounded-xl shadow-lg p-12 text-center">
                <div className="text-gray-400 mb-4">
                  <svg className="w-16 h-16 mx-auto" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9.172 16.172a4 4 0 015.656 0M9 12h6m-6-4h6m2 5.291A7.962 7.962 0 0112 15c-2.219 0-4.207.906-5.659 2.37M15 17h6l-3-3z" />
                  </svg>
                </div>
                <h3 className="text-xl font-bold text-gray-600 mb-2">No se encontraron resultados</h3>
                <p className="text-gray-500">Intenta con otros filtros o términos de búsqueda.</p>
                <button 
                  onClick={() => {
                    setSearchTerm('');
                    setSelectedDevType('');
                    setSelectedComplexity('');
                  }}
                  className="mt-4 bg-blue-600 text-white px-4 py-2 rounded-lg hover:bg-blue-700 transition-colors"
                >
                  Limpiar filtros
                </button>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Footer */}
      <footer className="bg-gray-800 text-white py-8 mt-16">
        <div className="container mx-auto px-6 text-center">
          <p className="text-gray-400">© 2025 ABAP Prompts Wiki - Los mejores prompts optimizados para Copilot 365</p>
          <p className="text-gray-500 text-sm mt-2">
            🤖 Cada prompt está diseñado para generar código ABAP de alta calidad usando IA
          </p>
        </div>
      </footer>
    </div>
  );
}

export default App;