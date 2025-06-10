import React, { useState, useEffect } from 'react';
import './App.css';

const abapPrompts = {
  "seleccion-datos": {
    title: "Selección de Datos Clásica",
    icon: "🔍",
    prompts: [
      {
        title: "SELECT básico con WHERE",
        description: "Selección básica de datos con condiciones",
        code: `SELECT * FROM mara
  INTO TABLE @DATA(lt_mara)
  WHERE mtart = 'FERT'
    AND created_on >= @sy-datum.`,
        tags: ["SELECT", "WHERE", "INTERNAL TABLE"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "JOIN de tablas",
        description: "Unión de múltiples tablas con INNER JOIN",
        code: `SELECT m~matnr, m~mtart, t~maktx
  FROM mara AS m
  INNER JOIN makt AS t ON m~matnr = t~matnr
  INTO TABLE @DATA(lt_material)
  WHERE m~mtart = 'FERT'
    AND t~spras = @sy-langu.`,
        tags: ["JOIN", "INNER JOIN", "ALIAS"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "SELECT con manejo de errores",
        description: "Consulta con validación y manejo de excepciones para correctivos",
        code: `SELECT SINGLE * FROM mara
  INTO @DATA(ls_mara)
  WHERE matnr = @lv_material.

IF sy-subrc <> 0.
  MESSAGE e001(z_custom) WITH 'Material' lv_material 'not found'.
  RETURN.
ENDIF.

" Validación adicional para correctivo
IF ls_mara-lvorm = 'X'.
  MESSAGE e002(z_custom) WITH 'Material' lv_material 'is deleted'.
  RETURN.
ENDIF.`,
        tags: ["ERROR HANDLING", "VALIDATION", "MESSAGE"],
        developmentType: "correctivo",
        complexity: "intermedio"
      },
      {
        title: "SELECT optimizado para performance",
        description: "Consulta optimizada con índices y FOR ALL ENTRIES",
        code: `" Verificar tabla no vacía antes de FOR ALL ENTRIES
IF lt_vbeln IS NOT INITIAL.
  
  " Usar SELECT con campos específicos para optimizar
  SELECT vbeln, posnr, matnr, kwmeng, netwr
    FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_vbeln
    WHERE vbeln = @lt_vbeln-vbeln
      AND abgru = ''  " Solo ítems no rechazados
    ORDER BY vbeln, posnr.
    
  " Usar SORT y READ TABLE para búsquedas eficientes
  SORT lt_items BY vbeln posnr.
  
ENDIF.`,
        tags: ["PERFORMANCE", "FOR ALL ENTRIES", "OPTIMIZATION"],
        developmentType: "optimizacion",
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
        description: "Estructura básica de una vista CDS para nuevo desarrollo",
        code: `@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Master Data'

define view Z_I_MATERIAL_BASIC
  as select from mara
{
  key mara.matnr as Material,
      mara.mtart as MaterialType,
      mara.mbrsh as IndustryStandard,
      mara.meins as BaseUnit,
      mara.created_on as CreatedOn
}`,
        tags: ["CDS", "@AbapCatalog", "@AccessControl"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "CDS con Associations",
        description: "Vista CDS con asociaciones para desarrollo evolutivo",
        code: `@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material with Enhanced Data'

define view Z_I_MATERIAL_ENHANCED
  as select from mara
  association [1..*] to makt as _Text on mara.matnr = _Text.matnr
  association [0..1] to marm as _UoM on mara.matnr = _UoM.matnr
{
  key mara.matnr as Material,
      mara.mtart as MaterialType,
      mara.meins as BaseUnit,
      
      // Campos añadidos en evolutivo
      mara.bstme as PurchaseUnit,
      mara.volum as Volume,
      
      // Associations
      _Text,
      _UoM
}`,
        tags: ["CDS", "Association", "Enhancement"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "CDS View con corrección de datos",
        description: "Vista CDS con lógica para corregir datos inconsistentes",
        code: `@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Data Correction'

define view Z_I_MATERIAL_CORRECTED
  as select from mara
{
  key mara.matnr as Material,
      mara.mtart as MaterialType,
      
      // Corrección: Si BaseUnit está vacío, usar EA como default
      case 
        when mara.meins = '' then 'EA'
        else mara.meins
      end as BaseUnit,
      
      // Corrección: Normalizar estado de material
      case mara.lvorm
        when 'X' then 'DELETED'
        when ''  then 'ACTIVE'
        else 'UNKNOWN'
      end as MaterialStatus,
      
      mara.created_on as CreatedOn
}
where mara.matnr <> ''`,
        tags: ["CDS", "Data Correction", "CASE", "Normalization"],
        developmentType: "correctivo",
        complexity: "intermedio"
      },
      {
        title: "Analytical CDS optimizada",
        description: "Vista analítica optimizada para reporting de alto rendimiento",
        code: `@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL
@EndUserText.label: 'Optimized Sales Analytics'

define view Z_C_SALES_ANALYTICS_OPT
  as select from vbap as sales
  association [1] to vbak as _Header on sales.vbeln = _Header.vbeln
{
  @Analytics.dimension: true
  sales.vbeln as SalesDocument,
  
  @Analytics.dimension: true
  _Header.kunnr as Customer,
  
  @Analytics.dimension: true
  _Header.vkorg as SalesOrg,
  
  // Optimización: Pre-calcular indicadores
  @Analytics.measure: true
  @Aggregation.default: #SUM
  sales.netwr as NetValue,
  
  @Analytics.measure: true
  @Aggregation.default: #SUM
  sales.kwmeng as Quantity,
  
  // Nuevo KPI calculado
  @Analytics.measure: true
  @Aggregation.default: #AVG
  division( sales.netwr, sales.kwmeng, 2 ) as UnitPrice
}
where sales.abgru = ''  // Solo items no rechazados para performance`,
        tags: ["@Analytics", "@Aggregation", "Performance", "KPI"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      }
    ]
  },
  "sap-rap": {
    title: "SAP RAP (Restful ABAP Programming)",
    icon: "🚀",
    prompts: [
      {
        title: "Behavior Definition - Managed Básico",
        description: "Definición de comportamiento básica para nuevo desarrollo RAP",
        code: `managed implementation in class ZCL_BP_TRAVEL unique;
strict ( 2 );
with draft;

define behavior for Z_I_TRAVEL alias Travel
persistent table ZTRAVEL
draft table ZTRAVELD
etag master LocalLastChanged
lock master total etag LastChanged
authorization master( global )
{
  field ( readonly )
   TravelId,
   LocalCreatedBy,
   LocalCreatedAt,
   LocalLastChanged,
   LastChanged;

  field ( numbering : managed )
   TravelId;

  create;
  update;
  delete;

  draft action Edit;
  draft action Activate optimized;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;

  mapping for ZTRAVEL
  {
    TravelId = travel_id;
    Description = description;
  }
}`,
        tags: ["Behavior Definition", "managed", "draft", "basic"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "RAP con Actions y Determinations",
        description: "Behavior Definition avanzado con actions personalizadas",
        code: `managed implementation in class ZCL_BP_TRAVEL_ENHANCED unique;
strict ( 2 );
with draft;

define behavior for Z_I_TRAVEL_ENHANCED alias Travel
persistent table ZTRAVEL
draft table ZTRAVELD
etag master LocalLastChanged
lock master total etag LastChanged
authorization master( global )
{
  field ( readonly )
   TravelId,
   TotalPrice,
   LocalLastChanged;

  field ( numbering : managed )
   TravelId;

  create;
  update;
  delete;

  // Actions personalizadas
  action acceptTravel result [1] $self;
  action rejectTravel result [1] $self;
  action calculateTotal result [1] $self;
  
  // Determinations
  determination calculateTotalPrice on modify { field BookingFee, CurrencyCode; }
  determination setTravelNumber on save { create; }
  
  // Validations
  validation validateCustomer on save { field CustomerId; }
  validation validateDates on save { field BeginDate, EndDate; }
  validation validatePrice on save { field BookingFee; }

  // Side effects para actualización automática
  side effects
  {
    field BookingFee affects field TotalPrice;
    field CurrencyCode affects field TotalPrice;
  }

  mapping for ZTRAVEL
  {
    TravelId = travel_id;
    CustomerId = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    BookingFee = booking_fee;
    TotalPrice = total_price;
    CurrencyCode = currency_code;
  }
}`,
        tags: ["Actions", "Determinations", "Validations", "Side Effects"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "RAP Error Handling",
        description: "Manejo de errores y mensajes en RAP para correctivos",
        code: `CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      validateCustomer FOR VALIDATE ON SAVE
        IMPORTING keys FOR Travel~validateCustomer,
      validateDates FOR VALIDATE ON SAVE
        IMPORTING keys FOR Travel~validateDates.
ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.
  METHOD validateCustomer.
    READ ENTITIES OF z_i_travel_enhanced IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      " Validar que cliente existe
      SELECT SINGLE @abap_true FROM kna1
        WHERE kunnr = @travel-CustomerId
        INTO @DATA(customer_exists).
        
      IF customer_exists = abap_false.
        APPEND VALUE #( %tky = travel-%tky
                       %element-CustomerId = if_abap_behv=>mk-on ) 
          TO failed-travel.
          
        APPEND VALUE #( %tky = travel-%tky
                       %element-CustomerId = if_abap_behv=>mk-on
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text = |Customer { travel-CustomerId } does not exist| ) )
          TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Error Handling", "Validation", "Messages", "READ ENTITIES"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "odata-services": {
    title: "OData Services",
    icon: "🌐",
    prompts: [
      {
        title: "Service Definition básico",
        description: "Definición de servicio OData para nuevo desarrollo Fiori",
        code: `@EndUserText.label: 'Travel Service'
define service Z_UI_TRAVEL_O4 {
  expose Z_C_TRAVEL as Travel;
  expose Z_C_BOOKING as Booking;
  expose Z_I_CUSTOMER as Customer;
  expose Z_I_AGENCY as TravelAgency;
}`,
        tags: ["Service Definition", "expose", "OData", "Fiori"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "Service Binding con configuración",
        description: "Binding de servicio con configuraciones específicas",
        code: `" En Service Binding
" Configuración de servicio con autenticación y autorización

" Metadata Extensions para el servicio
@Metadata.layer: #CUSTOMER
@UI: {
  headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title: {
      type: #STANDARD,
      value: 'TravelId'
    },
    description: {
      type: #STANDARD,
      value: 'Description'
    }
  }
}

annotate view Z_C_TRAVEL with
{
  @UI.facet: [ {
    id: 'idGeneralInformation',
    type: #COLLECTION,
    label: 'General Information',
    position: 10
  }, {
    id: 'idBookings',
    type: #LINEITEM_REFERENCE,
    label: 'Bookings',
    position: 20,
    targetElement: '_Booking'
  } ]

  @UI.lineItem: [ {
    position: 10,
    importance: #HIGH
  } ]
  TravelId;
}`,
        tags: ["Service Binding", "Metadata", "UI Annotations"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "OData con Custom Actions",
        description: "Servicio OData con acciones personalizadas para funcionalidad específica",
        code: `@EndUserText.label: 'Enhanced Travel Service'
define service Z_UI_TRAVEL_ENHANCED_O4 {
  expose Z_C_TRAVEL_ENHANCED as Travel
  {
    create;
    update;
    delete;
    
    // Custom Actions
    action acceptTravel;
    action rejectTravel;
    action copyTravel returns Z_C_TRAVEL_ENHANCED;
    
    // Function Import
    function calculatePrice returns TotalPrice;
  }
  
  expose Z_C_BOOKING_ENHANCED as Booking
  {
    create;
    update;
    delete;
    
    // Association navigation
    association _Travel;
  }
  
  // Value Help entities
  expose Z_I_CUSTOMER as Customer;
  expose Z_I_CURRENCY as Currency;
}`,
        tags: ["Custom Actions", "Function Import", "Association", "Value Help"],
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
        description: "Configuración básica de workflow para aprobaciones",
        code: `" Clase de workflow
CLASS zcl_workflow_travel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_workflow.
    
    METHODS: constructor
      IMPORTING
        iv_travel_id TYPE z_travel_id,
      
      start_approval_process
        RETURNING
          VALUE(rv_workitem_id) TYPE sww_wiid,
          
      complete_approval
        IMPORTING
          iv_approved TYPE abap_bool
          iv_comments TYPE string OPTIONAL.

  PRIVATE SECTION.
    DATA: mv_travel_id TYPE z_travel_id.
ENDCLASS.

CLASS zcl_workflow_travel IMPLEMENTATION.
  METHOD start_approval_process.
    " Iniciar workflow de aprobación
    DATA: lo_workflow TYPE REF TO cl_swf_run_wf_engine.
    
    CREATE OBJECT lo_workflow.
    
    " Parámetros del workflow
    DATA(lt_parameters) = VALUE swfparameters(
      ( name = 'TravelId' value = mv_travel_id )
      ( name = 'Requester' value = sy-uname )
    ).
    
    " Iniciar workflow
    CALL METHOD lo_workflow->start_workflow
      EXPORTING
        wi_id = 'WS99900001'  " Template de workflow
        parameters = lt_parameters
      RECEIVING
        workitem_id = rv_workitem_id.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Workflow", "Approval", "Business Process"],
        developmentType: "nuevo",
        complexity: "avanzado"
      },
      {
        title: "Event-based Workflow",
        description: "Workflow basado en eventos para automatización de procesos",
        code: `" Event handler para workflow automático
CLASS zcl_travel_event_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS: handle_travel_created
      FOR EVENT travel_created OF zcl_travel_manager
      IMPORTING
        sender
        travel_id
        total_amount.

  PRIVATE SECTION.
    CLASS-METHODS: trigger_approval_workflow
      IMPORTING
        iv_travel_id TYPE z_travel_id
        iv_amount TYPE z_amount.
ENDCLASS.

CLASS zcl_travel_event_handler IMPLEMENTATION.
  METHOD handle_travel_created.
    " Determinar si requiere aprobación
    IF total_amount > 1000.
      trigger_approval_workflow( 
        iv_travel_id = travel_id
        iv_amount = total_amount ).
    ENDIF.
  ENDMETHOD.
  
  METHOD trigger_approval_workflow.
    " Crear workitem automáticamente
    DATA(lo_workflow) = NEW zcl_workflow_travel( iv_travel_id ).
    DATA(lv_workitem) = lo_workflow->start_approval_process( ).
    
    " Log del proceso
    MESSAGE i001(z_workflow) WITH 'Approval workflow started' lv_workitem.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Event Handler", "Automatic Workflow", "Business Rules"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "Workflow Error Recovery",
        description: "Manejo de errores y recuperación en workflows para correctivos",
        code: `" Clase para manejo de errores en workflow
CLASS zcl_workflow_error_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS: handle_workflow_error
      IMPORTING
        iv_workitem_id TYPE sww_wiid
        iv_error_code TYPE sy-msgno
        iv_error_message TYPE string,
        
      retry_failed_workitem
        IMPORTING
          iv_workitem_id TYPE sww_wiid
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
          
      cancel_workflow
        IMPORTING
          iv_workitem_id TYPE sww_wiid
          iv_reason TYPE string.

  PRIVATE SECTION.
    METHODS: log_error
      IMPORTING
        iv_workitem_id TYPE sww_wiid
        iv_message TYPE string.
ENDCLASS.

CLASS zcl_workflow_error_handler IMPLEMENTATION.
  METHOD handle_workflow_error.
    " Log del error
    log_error( 
      iv_workitem_id = iv_workitem_id
      iv_message = |Error { iv_error_code }: { iv_error_message }| ).
    
    " Intentar recuperación automática
    IF iv_error_code = '001'.  " Error recoverable
      DATA(lv_retry_success) = retry_failed_workitem( iv_workitem_id ).
      IF lv_retry_success = abap_false.
        " Escalar a administrador
        MESSAGE i002(z_workflow) WITH 'Workflow escalated to admin' iv_workitem_id.
      ENDIF.
    ELSE.
      " Error crítico - cancelar workflow
      cancel_workflow( 
        iv_workitem_id = iv_workitem_id
        iv_reason = iv_error_message ).
    ENDIF.
  ENDMETHOD.
  
  METHOD retry_failed_workitem.
    " Implementar lógica de retry
    TRY.
        " Reintentar operación
        DATA(lo_workitem) = cl_swf_run_wf_engine=>get_instance( iv_workitem_id ).
        lo_workitem->restart( ).
        rv_success = abap_true.
        
      CATCH cx_swf_run_wf_base INTO DATA(lx_error).
        rv_success = abap_false.
        log_error( 
          iv_workitem_id = iv_workitem_id
          iv_message = lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Error Handling", "Recovery", "Workflow Admin", "Exception"],
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
        title: "AMDP básico",
        description: "Procedimiento AMDP básico para optimización de consultas",
        code: `CLASS zcl_amdp_material_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.
    
    CLASS-METHODS: get_material_hierarchy
      IMPORTING
        VALUE(iv_plant) TYPE werks_d
        VALUE(iv_material_type) TYPE mtart
      EXPORTING
        VALUE(et_result) TYPE ztt_material_hierarchy.

ENDCLASS.

CLASS zcl_amdp_material_data IMPLEMENTATION.
  METHOD get_material_hierarchy
    BY DATABASE PROCEDURE
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING mara marm marc.
    
    -- Consulta optimizada en HANA
    et_result = 
      SELECT m.matnr as material,
             m.mtart as material_type,
             m.meins as base_unit,
             c.werks as plant,
             c.dismm as mrp_type,
             u.meinh as alt_unit,
             u.umrez as numerator,
             u.umren as denominator
      FROM mara as m
      LEFT JOIN marc as c ON m.matnr = c.matnr
      LEFT JOIN marm as u ON m.matnr = u.matnr
      WHERE c.werks = :iv_plant
        AND m.mtart = :iv_material_type
        AND m.lvorm = ''
      ORDER BY m.matnr, u.meinh;
      
  ENDMETHOD.
ENDCLASS.`,
        tags: ["AMDP", "SQLSCRIPT", "HANA", "Performance"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      },
      {
        title: "AMDP con Table Functions",
        description: "Function AMDP para cálculos complejos en HANA",
        code: `CLASS zcl_amdp_analytics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.
    
    CLASS-METHODS: calculate_sales_metrics
      IMPORTING
        VALUE(iv_from_date) TYPE dats
        VALUE(iv_to_date) TYPE dats
        VALUE(iv_sales_org) TYPE vkorg
      RETURNING
        VALUE(rt_metrics) TYPE ztt_sales_metrics.

ENDCLASS.

CLASS zcl_amdp_analytics IMPLEMENTATION.
  METHOD calculate_sales_metrics
    BY DATABASE FUNCTION
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING vbak vbap knvv.
    
    -- Variables locales
    DECLARE lv_current_period INT;
    DECLARE lv_previous_period INT;
    
    -- Calcular períodos
    lv_current_period := YEAR(:iv_to_date) * 100 + MONTH(:iv_to_date);
    lv_previous_period := CASE 
      WHEN MONTH(:iv_to_date) = 1 
      THEN (YEAR(:iv_to_date) - 1) * 100 + 12
      ELSE YEAR(:iv_to_date) * 100 + (MONTH(:iv_to_date) - 1)
    END;
    
    -- Métricas actuales
    current_sales = 
      SELECT h.kunnr as customer,
             SUM(i.netwr) as current_revenue,
             COUNT(DISTINCT h.vbeln) as current_orders,
             AVG(i.netwr) as avg_order_value
      FROM vbak as h
      INNER JOIN vbap as i ON h.vbeln = i.vbeln
      WHERE h.vkorg = :iv_sales_org
        AND h.erdat BETWEEN :iv_from_date AND :iv_to_date
      GROUP BY h.kunnr;
    
    -- Comparación con período anterior
    return 
      SELECT c.customer,
             c.current_revenue,
             c.current_orders,
             c.avg_order_value,
             -- Cálculo de growth rate
             CASE 
               WHEN LAG(c.current_revenue) OVER (PARTITION BY c.customer ORDER BY lv_current_period) > 0
               THEN ((c.current_revenue - LAG(c.current_revenue) OVER (PARTITION BY c.customer ORDER BY lv_current_period)) / 
                     LAG(c.current_revenue) OVER (PARTITION BY c.customer ORDER BY lv_current_period)) * 100
               ELSE 0
             END as growth_rate
      FROM :current_sales as c;
      
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Table Functions", "Analytics", "Window Functions", "Complex Calculations"],
        developmentType: "nuevo",
        complexity: "avanzado"
      },
      {
        title: "AMDP Error Handling",
        description: "AMDP con manejo de errores y validaciones",
        code: `CLASS zcl_amdp_robust DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.
    
    CLASS-METHODS: process_financial_data
      IMPORTING
        VALUE(iv_company_code) TYPE bukrs
        VALUE(iv_fiscal_year) TYPE gjahr
      EXPORTING
        VALUE(et_result) TYPE ztt_financial_data
        VALUE(et_errors) TYPE ztt_error_log.

ENDCLASS.

CLASS zcl_amdp_robust IMPLEMENTATION.
  METHOD process_financial_data
    BY DATABASE PROCEDURE
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING bkpf bseg t001.
    
    -- Declarar handler de excepciones
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
      et_errors = SELECT 
        'AMDP_ERROR' as error_type,
        ::SQL_ERROR_CODE as error_code,
        ::SQL_ERROR_MESSAGE as error_message,
        CURRENT_TIMESTAMP as error_timestamp
      FROM DUMMY;
      RETURN;
    END;
    
    -- Validar parámetros de entrada
    SELECT COUNT(*) INTO lv_company_exists 
    FROM t001 
    WHERE bukrs = :iv_company_code;
    
    IF :lv_company_exists = 0 THEN
      et_errors = SELECT 
        'VALIDATION_ERROR' as error_type,
        '001' as error_code,
        'Company code does not exist: ' || :iv_company_code as error_message,
        CURRENT_TIMESTAMP as error_timestamp
      FROM DUMMY;
      RETURN;
    END IF;
    
    -- Procesamiento principal con validaciones
    et_result = 
      SELECT h.bukrs as company_code,
             h.gjahr as fiscal_year,
             h.belnr as document_number,
             i.buzei as line_item,
             i.hkont as gl_account,
             i.dmbtr as amount_local,
             i.wrbtr as amount_document,
             -- Validación de datos
             CASE 
               WHEN i.dmbtr = 0 THEN 'ZERO_AMOUNT'
               WHEN i.hkont = '' THEN 'MISSING_ACCOUNT'
               ELSE 'OK'
             END as data_quality
      FROM bkpf as h
      INNER JOIN bseg as i ON h.mandt = i.mandt 
                          AND h.bukrs = i.bukrs 
                          AND h.belnr = i.belnr 
                          AND h.gjahr = i.gjahr
      WHERE h.bukrs = :iv_company_code
        AND h.gjahr = :iv_fiscal_year
        -- Filtros adicionales para calidad de datos
        AND h.stblg = ''  -- No es documento de reversa
        AND i.koart IN ('S', 'K', 'D')  -- Solo cuentas principales
      ORDER BY h.belnr, i.buzei;
      
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Error Handling", "Data Validation", "Robust Processing"],
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
        title: "ALV Grid Display",
        description: "Mostrar datos en formato grid ALV para reportes básicos",
        code: `DATA: lo_alv TYPE REF TO cl_gui_alv_grid,
      lo_container TYPE REF TO cl_gui_container.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program       = sy-repid
    i_callback_pf_status_set = 'SET_PF_STATUS'
    i_callback_user_command  = 'USER_COMMAND'
    i_grid_title            = 'Material Master Data'
    is_layout               = ls_layout
    it_fieldcat             = lt_fieldcat
    it_sort                 = lt_sort
  TABLES
    t_outtab                = lt_material
  EXCEPTIONS
    program_error           = 1
    OTHERS                  = 2.`,
        tags: ["ALV", "REUSE_ALV_GRID_DISPLAY", "fieldcat"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "ALV con funcionalidades avanzadas",
        description: "ALV con filtros, sorting y export para evolutivos",
        code: `DATA: lo_salv TYPE REF TO cl_salv_table.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table = lo_salv
  CHANGING
    t_table      = lt_data ).

" Habilitar todas las funciones
lo_salv->get_functions( )->set_all( ).

" Configurar columnas con formato específico
DATA(lo_columns) = lo_salv->get_columns( ).
lo_columns->set_optimize( ).

" Configurar columna específica
DATA(lo_column) = lo_columns->get_column( 'NETWR' ).
lo_column->set_currency_column( 'WAERS' ).
lo_column->set_long_text( 'Net Value' ).

" Configurar filtros automáticos
DATA(lo_filter) = lo_salv->get_filters( ).
lo_filter->add_filter( 
  columnname = 'MTART'
  value = 'FERT' ).

" Configurar eventos
DATA(lo_events) = lo_salv->get_event( ).
SET HANDLER lcl_event_handler=>on_link_click FOR lo_events.

" Mostrar
lo_salv->display( ).`,
        tags: ["cl_salv_table", "Advanced Features", "Events", "Filters"],
        developmentType: "evolutivo",
        complexity: "intermedio"
      },
      {
        title: "ALV con corrección de datos",
        description: "ALV con validación y corrección de datos para correctivos",
        code: `CLASS lcl_alv_validator DEFINITION.
  PUBLIC SECTION.
    METHODS: validate_and_display
      CHANGING
        ct_data TYPE ztt_material_data.
        
  PRIVATE SECTION.
    METHODS: validate_data
      CHANGING
        ct_data TYPE ztt_material_data,
      fix_data_issues
      CHANGING
        ct_data TYPE ztt_material_data,
      display_with_validation
      IMPORTING
        it_data TYPE ztt_material_data.
ENDCLASS.

CLASS lcl_alv_validator IMPLEMENTATION.
  METHOD validate_and_display.
    " Validar datos antes de mostrar
    validate_data( CHANGING ct_data = ct_data ).
    
    " Corregir problemas encontrados
    fix_data_issues( CHANGING ct_data = ct_data ).
    
    " Mostrar con indicadores de calidad
    display_with_validation( ct_data ).
  ENDMETHOD.
  
  METHOD validate_data.
    LOOP AT ct_data INTO DATA(ls_data).
      " Validaciones específicas
      IF ls_data-meins = ''.
        ls_data-status = 'ERROR'.
        ls_data-message = 'Missing base unit'.
      ELSEIF ls_data-netwr < 0.
        ls_data-status = 'WARNING'.
        ls_data-message = 'Negative value detected'.
      ELSE.
        ls_data-status = 'OK'.
        CLEAR ls_data-message.
      ENDIF.
      
      MODIFY ct_data FROM ls_data.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Data Validation", "Error Correction", "Quality Control"],
        developmentType: "correctivo",
        complexity: "intermedio"
      }
    ]
  },
  "forms": {
    title: "Formularios",
    icon: "📄",
    prompts: [
      {
        title: "SmartForm - Definición",
        description: "Estructura básica de un SmartForm para nuevo desarrollo",
        code: `" Llamada a SmartForm
DATA: lv_fm_name TYPE rs38l_fnam.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = 'ZSF_INVOICE'
  IMPORTING
    fm_name            = lv_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

CALL FUNCTION lv_fm_name
  EXPORTING
    control_parameters = ls_control
    output_options     = ls_output
    user_settings      = ' '
    is_header         = ls_header
    it_items          = lt_items
  EXCEPTIONS
    formatting_error   = 1
    internal_error     = 2
    send_error         = 3
    user_canceled      = 4
    OTHERS             = 5.`,
        tags: ["SmartForm", "SSF_FUNCTION_MODULE_NAME", "control_parameters"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "Adobe Forms con datos dinámicos",
        description: "Formulario Adobe con contenido dinámico para evolutivos",
        code: `" Llamada a Adobe Form con datos dinámicos
DATA: lo_fp TYPE REF TO if_fp,
      lo_pdfobj TYPE REF TO if_fp_pdf_object,
      lo_usage TYPE REF TO if_fp_usage.

" Obtener instancia del Form Processing
lo_fp = cl_fp=>get_reference( ).

" Configurar uso del formulario
lo_usage = lo_fp->create_usage( ).
lo_usage->set_formtemplate_name( 'ZFP_TRAVEL_FORM' ).
lo_usage->set_programming_language( if_fp_usage=>c_lang_abap ).

" Crear objeto PDF
lo_pdfobj = lo_usage->create_pdf_object( ).

" Configurar datos del formulario
DATA(ls_form_data) = VALUE zs_travel_form_data(
  travel_id = lv_travel_id
  customer_name = lv_customer
  total_amount = lv_amount
  currency = lv_currency
  items = lt_items
).

" Configurar parámetros dinámicos
LOOP AT lt_dynamic_fields INTO DATA(ls_field).
  lo_pdfobj->set_data( 
    name = ls_field-field_name
    value = ls_field-field_value ).
ENDLOOP.

" Generar PDF
lo_pdfobj->set_data( 
  name = 'FORM_DATA'
  value = ls_form_data ).

lo_pdfobj->execute( ).

" Obtener resultado
DATA(lv_pdf_data) = lo_pdfobj->get_pdf( ).`,
        tags: ["Adobe Forms", "Dynamic Data", "PDF Generation"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      }
    ]
  },
  "bapis": {
    title: "BAPIs y Function Modules",
    icon: "🔧",
    prompts: [
      {
        title: "BAPI Material - Crear",
        description: "Crear material usando BAPI para nuevo desarrollo",
        code: `DATA: ls_headdata TYPE bapimathead,
      ls_clientdata TYPE bapi_mara,
      lt_return TYPE TABLE OF bapiret2.

ls_headdata-material = 'TEST001'.
ls_headdata-ind_sector = 'M'.
ls_headdata-matl_type = 'FERT'.

ls_clientdata-base_uom = 'EA'.
ls_clientdata-division = '01'.

CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
  EXPORTING
    headdata    = ls_headdata
    clientdata  = ls_clientdata
  TABLES
    return      = lt_return.

IF line_exists( lt_return[ type = 'E' ] ).
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDIF.`,
        tags: ["BAPI", "BAPI_MATERIAL_SAVEDATA", "COMMIT", "ROLLBACK"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "BAPI con manejo robusto de errores",
        description: "BAPI con validación completa y manejo de errores para correctivos",
        code: `CLASS zcl_bapi_material_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: create_material
      IMPORTING
        is_material_data TYPE zs_material_create
      RETURNING
        VALUE(rs_result) TYPE zs_creation_result.
        
  PRIVATE SECTION.
    METHODS: validate_input_data
      IMPORTING
        is_data TYPE zs_material_create
      RETURNING
        VALUE(rt_errors) TYPE bapiret2_t,
      execute_bapi_with_retry
      IMPORTING
        is_headdata TYPE bapimathead
        is_clientdata TYPE bapi_mara
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.
ENDCLASS.

CLASS zcl_bapi_material_handler IMPLEMENTATION.
  METHOD create_material.
    " Validar datos de entrada
    DATA(lt_validation_errors) = validate_input_data( is_material_data ).
    
    IF lines( lt_validation_errors ) > 0.
      rs_result-success = abap_false.
      rs_result-messages = lt_validation_errors.
      RETURN.
    ENDIF.
    
    " Preparar datos para BAPI
    DATA(ls_headdata) = VALUE bapimathead(
      material = is_material_data-material
      ind_sector = is_material_data-industry
      matl_type = is_material_data-material_type ).
      
    " Ejecutar BAPI con retry logic
    DATA(lt_bapi_return) = execute_bapi_with_retry(
      is_headdata = ls_headdata
      is_clientdata = CORRESPONDING #( is_material_data ) ).
    
    " Analizar resultados
    rs_result-success = COND #( WHEN line_exists( lt_bapi_return[ type = 'E' ] )
                               THEN abap_false ELSE abap_true ).
    rs_result-messages = lt_bapi_return.
    
    IF rs_result-success = abap_true.
      rs_result-material_number = ls_headdata-material.
    ENDIF.
  ENDMETHOD.
  
  METHOD execute_bapi_with_retry.
    DATA: lv_retry_count TYPE i VALUE 0,
          lv_max_retries TYPE i VALUE 3.
          
    DO lv_max_retries TIMES.
      lv_retry_count = lv_retry_count + 1.
      
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata = is_headdata
          clientdata = is_clientdata
        TABLES
          return = rt_return.
          
      " Si no hay errores, salir del loop
      IF NOT line_exists( rt_return[ type = 'E' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        EXIT.
      ELSE.
        " Si es el último intento, no hacer rollback
        if lv_retry_count < lv_max_retries.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          WAIT UP TO 1 SECONDS.  " Pausa antes del retry
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Error Handling", "Retry Logic", "Validation", "Robust Processing"],
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
        title: "User-Exit Implementation",
        description: "Implementación básica de User-Exit para nuevo desarrollo",
        code: `" En include del User-Exit
CASE i_step.
  WHEN '001'.
    " Validación antes de grabar
    LOOP AT i_vbap INTO wa_vbap.
      IF wa_vbap-kwmeng > 1000.
        MESSAGE e001(z_custom) WITH 'Quantity exceeds limit'.
      ENDIF.
    ENDLOOP.
    
  WHEN '002'.
    " Modificación de datos
    LOOP AT c_vbap INTO wa_vbap.
      wa_vbap-zzfield = 'CUSTOM_VALUE'.
      MODIFY c_vbap FROM wa_vbap.
    ENDLOOP.
ENDCASE.`,
        tags: ["User-Exit", "CASE", "MESSAGE", "MODIFY"],
        developmentType: "nuevo",
        complexity: "intermedio"
      },
      {
        title: "Enhancement Spot con filtros",
        description: "Enhancement spot con lógica de filtrado para evolutivos",
        code: `" Enhancement Spot Implementation
CLASS zcl_enh_sales_document DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_ex_sales_document_enh.
ENDCLASS.

CLASS zcl_enh_sales_document IMPLEMENTATION.
  METHOD if_ex_sales_document_enh~change_item_data.
    " Lógica evolutiva para modificar items
    LOOP AT ct_vbap INTO DATA(ls_item).
      " Aplicar descuentos especiales por volumen
      IF ls_item-kwmeng >= 100.
        ls_item-zz_volume_discount = '10'.
        ls_item-netwr = ls_item-netwr * '0.9'.
      ELSEIF ls_item-kwmeng >= 50.
        ls_item-zz_volume_discount = '5'.
        ls_item-netwr = ls_item-netwr * '0.95'.
      ENDIF.
      
      " Validación de precios especiales
      IF ls_item-matnr IN rt_special_materials.
        " Aplicar precio especial desde tabla personalizada
        SELECT SINGLE special_price FROM ztable_special_prices
          INTO @DATA(lv_special_price)
          WHERE matnr = @ls_item-matnr
            AND vkorg = @lv_sales_org
            AND valid_from <= @sy-datum
            AND valid_to >= @sy-datum.
            
        IF sy-subrc = 0.
          ls_item-netwr = lv_special_price * ls_item-kwmeng.
        ENDIF.
      ENDIF.
      
      MODIFY ct_vbap FROM ls_item.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Enhancement Spot", "Business Logic", "Price Calculation"],
        developmentType: "evolutivo",
        complexity: "avanzado"
      },
      {
        title: "BAdI con validación de datos",
        description: "BAdI con validaciones específicas para correctivos",
        code: `CLASS zcl_badi_data_validation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_ex_material_badi.

  PRIVATE SECTION.
    METHODS: validate_material_data
      IMPORTING
        is_material TYPE mara
      RETURNING
        VALUE(rt_errors) TYPE bapiret2_t,
      fix_data_inconsistencies
      CHANGING
        cs_material TYPE mara.
ENDCLASS.

CLASS zcl_badi_data_validation IMPLEMENTATION.
  METHOD if_ex_material_badi~check_material.
    " Validaciones específicas para correctivos
    DATA(lt_errors) = validate_material_data( is_material ).
    
    " Si hay errores críticos, bloquear
    IF line_exists( lt_errors[ type = 'E' ] ).
      LOOP AT lt_errors INTO DATA(ls_error) WHERE type = 'E'.
        MESSAGE ID ls_error-id TYPE ls_error-type NUMBER ls_error-number
          WITH ls_error-message_v1 ls_error-message_v2 
               ls_error-message_v3 ls_error-message_v4.
      ENDLOOP.
    ENDIF.
    
    " Intentar corregir datos automáticamente
    fix_data_inconsistencies( CHANGING cs_material = cs_material ).
  ENDMETHOD.
  
  METHOD validate_material_data.
    " Validación 1: Base unit obligatoria
    IF is_material-meins = ''.
      APPEND VALUE #( type = 'E' id = 'Z_MATERIAL' number = '001'
                     message_v1 = 'Base unit is mandatory' ) TO rt_errors.
    ENDIF.
    
    " Validación 2: Peso debe ser coherente
    IF is_material-brgew > 0 AND is_material-gewei = ''.
      APPEND VALUE #( type = 'E' id = 'Z_MATERIAL' number = '002'
                     message_v1 = 'Weight unit missing' ) TO rt_errors.
    ENDIF.
    
    " Validación 3: Verificar duplicados por EAN
    IF is_material-ean11 <> ''.
      SELECT COUNT(*) FROM mara
        INTO @DATA(lv_count)
        WHERE ean11 = @is_material-ean11
          AND matnr <> @is_material-matnr.
          
      IF lv_count > 0.
        APPEND VALUE #( type = 'E' id = 'Z_MATERIAL' number = '003'
                       message_v1 = 'EAN already exists' 
                       message_v2 = is_material-ean11 ) TO rt_errors.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  
  METHOD fix_data_inconsistencies.
    " Corrección automática de datos
    IF cs_material-meins = '' AND cs_material-mtart = 'FERT'.
      cs_material-meins = 'EA'.  " Default unit para productos terminados
    ENDIF.
    
    " Normalizar campos de texto
    cs_material-maktx = cl_abap_char_utilities=>uppercase( cs_material-maktx ).
  ENDMETHOD.
ENDCLASS.`,
        tags: ["BAdI", "Data Validation", "Auto Correction", "Business Rules"],
        developmentType: "correctivo",
        complexity: "avanzado"
      }
    ]
  },
  "performance": {
    title: "Performance y Optimización",
    icon: "⚡",
    prompts: [
      {
        title: "SELECT optimizado básico",
        description: "Consulta básica optimizada para nuevo desarrollo",
        code: `" Verificar que la tabla interna no esté vacía
IF lt_vbeln IS NOT INITIAL.
  SELECT vbeln, posnr, matnr, kwmeng
    FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_vbeln
    WHERE vbeln = @lt_vbeln-vbeln.
ENDIF.

" Usar FILTER para búsquedas eficientes
DATA(lt_filtered) = FILTER #( lt_items USING KEY vbeln
  WHERE vbeln = lv_sales_doc ).`,
        tags: ["FOR ALL ENTRIES", "FILTER", "Performance"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "Optimización avanzada con índices",
        description: "Consulta altamente optimizada para mejorar performance existente",
        code: `" Clase para optimización avanzada de consultas
CLASS zcl_performance_optimizer DEFINITION.
  PUBLIC SECTION.
    METHODS: get_optimized_sales_data
      IMPORTING
        it_selection TYPE ztt_sales_selection
      RETURNING
        VALUE(rt_data) TYPE ztt_sales_data.
        
  PRIVATE SECTION.
    METHODS: build_optimized_query
      IMPORTING
        it_selection TYPE ztt_sales_selection
      RETURNING
        VALUE(rv_query) TYPE string,
      use_parallel_processing
      IMPORTING
        it_selection TYPE ztt_sales_selection
      RETURNING
        VALUE(rt_data) TYPE ztt_sales_data.
ENDCLASS.

CLASS zcl_performance_optimizer IMPLEMENTATION.
  METHOD get_optimized_sales_data.
    " Decidir estrategia basada en volumen de datos
    DATA(lv_selection_size) = lines( it_selection ).
    
    IF lv_selection_size > 10000.
      " Para grandes volúmenes, usar procesamiento paralelo
      rt_data = use_parallel_processing( it_selection ).
    ELSE.
      " Para volúmenes menores, usar consulta optimizada simple
      SELECT h~vbeln, h~kunnr, h~netwr,
             i~posnr, i~matnr, i~kwmeng
        FROM vbak AS h
        INNER JOIN vbap AS i ON h~vbeln = i~vbeln
        INTO CORRESPONDING FIELDS OF TABLE @rt_data
        FOR ALL ENTRIES IN @it_selection
        WHERE h~vbeln = @it_selection-vbeln
          AND h~vkorg IN @it_selection-sales_org_range
          AND h~erdat IN @it_selection-date_range
          " Usar índice específico
          AND h~auart IN ('ZOR1', 'ZOR2')
        ORDER BY h~vbeln, i~posnr.
    ENDIF.
    
    " Cache results para consultas repetidas
    zcl_cache_manager=>store_results( 
      iv_key = |SALES_{ lv_selection_size }|
      it_data = rt_data ).
  ENDMETHOD.
  
  METHOD use_parallel_processing.
    " Dividir selección en chunks para procesamiento paralelo
    DATA: lt_chunk1 TYPE ztt_sales_selection,
          lt_chunk2 TYPE ztt_sales_selection,
          lt_chunk3 TYPE ztt_sales_selection.
          
    " Dividir datos en 3 grupos
    DATA(lv_chunk_size) = lines( it_selection ) DIV 3.
    
    " Procesamiento asíncrono usando ABAP Push Channels
    CALL FUNCTION 'Z_PARALLEL_SALES_QUERY'
      STARTING NEW TASK 'TASK1'
      EXPORTING
        it_selection = lt_chunk1.
        
    CALL FUNCTION 'Z_PARALLEL_SALES_QUERY' 
      STARTING NEW TASK 'TASK2'
      EXPORTING
        it_selection = lt_chunk2.
        
    " Procesar tercer chunk en foreground
    " ... lógica de procesamiento paralelo
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Advanced Optimization", "Parallel Processing", "Caching", "Index Usage"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      },
      {
        title: "Memory Management",
        description: "Gestión eficiente de memoria para aplicaciones pesadas",
        code: `" Clase para gestión optimizada de memoria
CLASS zcl_memory_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: process_large_dataset
      IMPORTING
        iv_max_memory TYPE i DEFAULT 100  " MB
      CHANGING
        ct_data TYPE STANDARD TABLE.
        
  PRIVATE SECTION.
    METHODS: get_memory_usage
      RETURNING
        VALUE(rv_memory_mb) TYPE i,
      process_in_chunks
      CHANGING
        ct_data TYPE STANDARD TABLE,
      cleanup_memory.
ENDCLASS.

CLASS zcl_memory_manager IMPLEMENTATION.
  METHOD process_large_dataset.
    CONSTANTS: lc_chunk_size TYPE i VALUE 10000.
    
    DATA: lv_current_memory TYPE i,
          lv_processed_records TYPE i,
          lt_chunk TYPE STANDARD TABLE.
          
    " Procesar en chunks para evitar overflow de memoria
    DO.
      " Verificar uso de memoria actual
      lv_current_memory = get_memory_usage( ).
      
      IF lv_current_memory > iv_max_memory.
        " Liberar memoria si es necesario
        cleanup_memory( ).
        
        " Si aún excede el límite, procesar en chunks más pequeños
        IF get_memory_usage( ) > iv_max_memory.
          process_in_chunks( CHANGING ct_data = ct_data ).
          EXIT.
        ENDIF.
      ENDIF.
      
      " Procesar siguiente chunk
      CLEAR lt_chunk.
      LOOP AT ct_data INTO DATA(ls_data) FROM lv_processed_records + 1 
                                         TO lv_processed_records + lc_chunk_size.
        APPEND ls_data TO lt_chunk.
      ENDLOOP.
      
      IF lines( lt_chunk ) = 0.
        EXIT.  " No más datos para procesar
      ENDIF.
      
      " Procesar chunk actual
      " ... lógica de procesamiento
      
      lv_processed_records = lv_processed_records + lines( lt_chunk ).
      
      " Progreso para usuario
      cl_progress_indicator=>progress_indicate(
        i_text = |Processing record { lv_processed_records } of { lines( ct_data ) }|
        i_processed = lv_processed_records
        i_total = lines( ct_data ) ).
    ENDDO.
  ENDMETHOD.
  
  METHOD get_memory_usage.
    " Obtener uso actual de memoria (simplificado)
    CALL 'GET_MEMORY_USAGE'
      ID 'MEMORY' FIELD rv_memory_mb.
  ENDMETHOD.
  
  METHOD cleanup_memory.
    " Liberar memoria no utilizada
    CALL 'MEMORY_CLEANUP'.
    
    " Forzar garbage collection
    CALL 'ABAP_GARBAGE_COLLECT'.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Memory Management", "Chunk Processing", "Performance Monitoring"],
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
        title: "Breakpoints dinámicos",
        description: "Técnicas de debugging básico para nuevo desarrollo",
        code: `" Breakpoint condicional
IF sy-uname = 'DEVELOPER'.
  BREAK-POINT.
ENDIF.

" Logging para análisis
MESSAGE i001(z_log) WITH 'Processing material:' lv_matnr.

" Assert para validaciones
ASSERT lv_quantity > 0.

" Checkpoint para análisis de performance
BREAK-POINT ID z_debug_point.`,
        tags: ["BREAK-POINT", "ASSERT", "MESSAGE", "Debugging"],
        developmentType: "nuevo",
        complexity: "básico"
      },
      {
        title: "Advanced Debugging con SAT",
        description: "Debugging avanzado con herramientas de análisis para optimización",
        code: `" Clase para debugging avanzado y análisis de performance
CLASS zcl_advanced_debugger DEFINITION.
  PUBLIC SECTION.
    METHODS: start_performance_analysis
      IMPORTING
        iv_analysis_name TYPE string DEFAULT 'CUSTOM_ANALYSIS',
      end_performance_analysis
      RETURNING
        VALUE(rs_results) TYPE zs_performance_results,
      debug_with_conditions
      IMPORTING
        iv_condition TYPE string
        iv_variable_name TYPE string
        iv_expected_value TYPE any.
        
  PRIVATE SECTION.
    DATA: mv_analysis_handle TYPE sat_perf_analysis_handle,
          mv_start_time TYPE timestampl.
ENDCLASS.

CLASS zcl_advanced_debugger IMPLEMENTATION.
  METHOD start_performance_analysis.
    " Iniciar análisis de performance con SAT
    GET TIME STAMP FIELD mv_start_time.
    
    " Configurar SAT para análisis automático
    DATA(lo_sat) = cl_sat_analysis=>create( ).
    lo_sat->set_analysis_name( iv_analysis_name ).
    lo_sat->start_measurement( ).
    
    " Activar checkpoint groups específicos
    CALL 'DEBUG_SET_CHECKPOINT_GROUP'
      ID 'GROUP' FIELD 'ZPERFORMANCE'
      ID 'ACTIVE' FIELD 'X'.
      
    " Log del inicio
    cl_abap_trace_utils=>add_trace_entry(
      i_category = 'PERFORMANCE'
      i_text = |Analysis started: { iv_analysis_name }|
      i_timestamp = mv_start_time ).
  ENDMETHOD.
  
  METHOD end_performance_analysis.
    " Finalizar análisis y obtener resultados
    GET TIME STAMP FIELD DATA(lv_end_time).
    
    " Calcular tiempo transcurrido
    rs_results-duration_ms = cl_abap_tstmp=>subtract(
      tstmp1 = lv_end_time
      tstmp2 = mv_start_time ) / 1000.
      
    " Obtener estadísticas de memoria
    CALL 'GET_MEMORY_STATISTICS'
      ID 'MAX_USED' FIELD rs_results-max_memory_kb
      ID 'CURRENT' FIELD rs_results-current_memory_kb.
      
    " Obtener información de DB
    rs_results-db_selects = sy-dbcnt.
    
    " Generar recomendaciones automáticas
    IF rs_results-duration_ms > 5000.
      APPEND 'Consider optimization - execution time > 5 seconds' TO rs_results-recommendations.
    ENDIF.
    
    IF rs_results-db_selects > 100.
      APPEND 'Too many DB accesses - consider FOR ALL ENTRIES' TO rs_results-recommendations.
    ENDIF.
    
    " Log final
    cl_abap_trace_utils=>add_trace_entry(
      i_category = 'PERFORMANCE'
      i_text = |Analysis completed. Duration: { rs_results-duration_ms }ms|
      i_timestamp = lv_end_time ).
  ENDMETHOD.
  
  METHOD debug_with_conditions.
    " Dynamic breakpoint con condiciones complejas
    DATA: lv_condition_met TYPE abap_bool.
    
    " Evaluar condición dinámicamente
    CALL 'EVALUATE_EXPRESSION'
      ID 'EXPRESSION' FIELD iv_condition
      ID 'VARIABLE' FIELD iv_variable_name
      ID 'VALUE' FIELD iv_expected_value
      ID 'RESULT' FIELD lv_condition_met.
      
    IF lv_condition_met = abap_true.
      " Capturar contexto antes del breakpoint
      DATA(ls_context) = VALUE zs_debug_context(
        program = sy-repid
        line = sy-linno
        user = sy-uname
        timestamp = cl_abap_tstmp=>systemtstmp_syst2utc( sy-datum, sy-uzeit )
        condition = iv_condition
        variable_name = iv_variable_name
      ).
      
      " Guardar contexto para análisis posterior
      zcl_debug_logger=>log_context( ls_context ).
      
      " Activar breakpoint
      BREAK-POINT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["SAT Analysis", "Performance Debugging", "Dynamic Breakpoints", "Context Capture"],
        developmentType: "optimizacion",
        complexity: "avanzado"
      },
      {
        title: "Unit Testing con mocks",
        description: "Framework de testing robusto para correctivos y validaciones",
        code: `" Clase de test completa con mocks y validaciones
CLASS ltcl_comprehensive_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_material_processor,
          mo_mock_db TYPE REF TO zif_database_interface,
          mo_mock_bapi TYPE REF TO zif_bapi_interface.
    
    METHODS: setup,
             teardown,
             test_material_creation_success,
             test_material_creation_failure,
             test_data_validation,
             test_error_handling,
             test_rollback_scenario.
ENDCLASS.

CLASS ltcl_comprehensive_test IMPLEMENTATION.
  METHOD setup.
    " Crear mocks para dependencias externas
    mo_mock_db = cl_mockup_loader=>create_mock( 'ZIF_DATABASE_INTERFACE' ).
    mo_mock_bapi = cl_mockup_loader=>create_mock( 'ZIF_BAPI_INTERFACE' ).
    
    " Inyectar dependencias en la clase bajo test
    CREATE OBJECT mo_cut
      EXPORTING
        io_database = mo_mock_db
        io_bapi = mo_mock_bapi.
  ENDMETHOD.
  
  METHOD teardown.
    " Limpiar recursos después de cada test
    CLEAR: mo_cut, mo_mock_db, mo_mock_bapi.
  ENDMETHOD.
  
  METHOD test_material_creation_success.
    " Datos de prueba
    DATA(ls_test_material) = VALUE zs_material_data(
      material = 'TEST001'
      material_type = 'FERT'
      base_unit = 'EA'
    ).
    
    " Configurar mock para retornar éxito
    DATA(lt_success_return) = VALUE bapiret2_t(
      ( type = 'S' message = 'Material created successfully' )
    ).
    
    cl_mockup_loader=>configure_method_call(
      io_mock = mo_mock_bapi
      i_method_name = 'CREATE_MATERIAL'
      i_return_value = lt_success_return ).
    
    " Ejecutar método bajo test
    DATA(ls_result) = mo_cut->create_material( ls_test_material ).
    
    " Validaciones
    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = 'Material creation should succeed' ).
      
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-material_number
      exp = 'TEST001'
      msg = 'Material number should match input' ).
      
    " Verificar que el mock fue llamado correctamente
    cl_mockup_loader=>verify_method_call(
      io_mock = mo_mock_bapi
      i_method_name = 'CREATE_MATERIAL'
      i_times_called = 1 ).
  ENDMETHOD.
  
  METHOD test_error_handling.
    " Simular error en BAPI
    DATA(lt_error_return) = VALUE bapiret2_t(
      ( type = 'E' id = 'MATERIAL' number = '001' 
        message = 'Material already exists' message_v1 = 'TEST001' )
    ).
    
    cl_mockup_loader=>configure_method_call(
      io_mock = mo_mock_bapi
      i_method_name = 'CREATE_MATERIAL'
      i_return_value = lt_error_return ).
    
    " Datos que causarán error
    DATA(ls_duplicate_material) = VALUE zs_material_data(
      material = 'TEST001'
      material_type = 'FERT'
    ).
    
    " Ejecutar y validar manejo de error
    DATA(ls_result) = mo_cut->create_material( ls_duplicate_material ).
    
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Creation should fail for duplicate material' ).
      
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-messages
      msg = 'Error messages should be populated' ).
  ENDMETHOD.
  
  METHOD test_rollback_scenario.
    " Test que valida rollback en caso de error
    " Configurar secuencia de llamadas
    DATA(lt_partial_success) = VALUE bapiret2_t(
      ( type = 'S' message = 'Header created' )
      ( type = 'E' message = 'Client data error' )
    ).
    
    " Configurar mock para simular rollback
    cl_mockup_loader=>configure_method_sequence(
      io_mock = mo_mock_bapi
      it_method_calls = VALUE #(
        ( method_name = 'CREATE_MATERIAL' return_value = lt_partial_success )
        ( method_name = 'ROLLBACK_TRANSACTION' return_value = VALUE #( ) )
      ) ).
    
    " Ejecutar test
    DATA(ls_test_data) = VALUE zs_material_data( material = 'ROLLBACK_TEST' ).
    DATA(ls_result) = mo_cut->create_material( ls_test_data ).
    
    " Validar que rollback fue ejecutado
    cl_mockup_loader=>verify_method_call(
      io_mock = mo_mock_bapi
      i_method_name = 'ROLLBACK_TRANSACTION'
      i_times_called = 1 ).
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Unit Testing", "Mocking", "Error Scenarios", "Rollback Testing"],
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

  const copyToClipboard = (code, title) => {
    navigator.clipboard.writeText(code);
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
                <span className="text-white text-2xl font-bold">A</span>
              </div>
              <div>
                <h1 className="text-3xl font-bold text-gray-800">ABAP Prompts Wiki</h1>
                <p className="text-gray-600">Los mejores prompts para desarrollo ABAP moderno - {totalPrompts} prompts disponibles</p>
              </div>
            </div>
          </div>
        </div>
      </header>

      <div className="container mx-auto px-6 py-8">
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
                            onClick={() => copyToClipboard(prompt.code, prompt.title)}
                            className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg transition-colors duration-200 flex items-center space-x-2 ml-4"
                          >
                            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                            </svg>
                            <span>{copiedCode === prompt.title ? '¡Copiado!' : 'Copiar'}</span>
                          </button>
                        </div>
                        
                        <div className="bg-gray-900 rounded-lg overflow-hidden">
                          <div className="bg-gray-800 px-4 py-2 flex items-center justify-between">
                            <span className="text-gray-300 text-sm font-medium">ABAP Code</span>
                            <div className="flex space-x-2">
                              <div className="w-3 h-3 bg-red-500 rounded-full"></div>
                              <div className="w-3 h-3 bg-yellow-500 rounded-full"></div>
                              <div className="w-3 h-3 bg-green-500 rounded-full"></div>
                            </div>
                          </div>
                          <pre className="p-4 text-sm text-green-400 font-mono overflow-x-auto">
                            <code>{prompt.code}</code>
                          </pre>
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
          <p className="text-gray-400">© 2025 ABAP Prompts Wiki - Los mejores recursos para desarrollo ABAP moderno</p>
          <p className="text-gray-500 text-sm mt-2">
            Incluye: CDS Views, SAP RAP, OData, Workflow, AMDP y técnicas de optimización
          </p>
        </div>
      </footer>
    </div>
  );
}

export default App;