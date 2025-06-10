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
        tags: ["SELECT", "WHERE", "INTERNAL TABLE"]
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
        tags: ["JOIN", "INNER JOIN", "ALIAS"]
      },
      {
        title: "SELECT con agregación",
        description: "Funciones de agregación SUM, COUNT, MAX",
        code: `SELECT vbeln, SUM( netwr ) AS total_amount,
         COUNT(*) AS line_count
  FROM vbap
  INTO TABLE @DATA(lt_summary)
  WHERE vbeln IN @lr_vbeln
  GROUP BY vbeln
  HAVING SUM( netwr ) >  1000.`,
        tags: ["SUM", "COUNT", "GROUP BY", "HAVING"]
      }
    ]
  },
  "cds-views": {
    title: "CDS Views",
    icon: "📊",
    prompts: [
      {
        title: "CDS View básica",
        description: "Estructura básica de una vista CDS",
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
        tags: ["CDS", "@AbapCatalog", "@AccessControl"]
      },
      {
        title: "CDS con Associations",
        description: "Vista CDS con asociaciones a otras entidades",
        code: `@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material with Text'

define view Z_I_MATERIAL_TEXT
  as select from mara
  association [1..*] to makt as _Text on mara.matnr = _Text.matnr
{
  key mara.matnr as Material,
      mara.mtart as MaterialType,
      mara.meins as BaseUnit,
      
      // Associations
      _Text
}`,
        tags: ["CDS", "Association", "_Text"]
      },
      {
        title: "Analytical CDS View",
        description: "Vista analítica con medidas y dimensiones",
        code: `@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL
@EndUserText.label: 'Sales Analytics'

define view Z_C_SALES_ANALYTICS
  as select from vbap as sales
  association [1] to vbak as _Header on sales.vbeln = _Header.vbeln
{
  @Analytics.dimension: true
  sales.vbeln as SalesDocument,
  
  @Analytics.dimension: true
  _Header.kunnr as Customer,
  
  @Analytics.measure: true
  @Aggregation.default: #SUM
  sales.netwr as NetValue,
  
  @Analytics.measure: true
  @Aggregation.default: #SUM
  sales.kwmeng as Quantity
}`,
        tags: ["@Analytics", "@Aggregation", "CUBE"]
      }
    ]
  },
  "sap-rap": {
    title: "SAP RAP (Restful ABAP Programming)",
    icon: "🚀",
    prompts: [
      {
        title: "Behavior Definition - Managed",
        description: "Definición de comportamiento para BO managed",
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

  validation validateCustomer on save { field CustomerId; }
  validation validateDates on save { field BeginDate, EndDate; }

  mapping for ZTRAVEL
  {
    TravelId = travel_id;
    CustomerId = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    Description = description;
  }
}`,
        tags: ["Behavior Definition", "managed", "draft", "validation"]
      },
      {
        title: "Service Definition",
        description: "Definición de servicio OData para exposición",
        code: `@EndUserText.label: 'Travel Service'
define service Z_UI_TRAVEL_O4 {
  expose Z_C_TRAVEL as Travel;
  expose Z_C_BOOKING as Booking;
  expose Z_I_CUSTOMER as Customer;
  expose Z_I_AGENCY as TravelAgency;
}`,
        tags: ["Service Definition", "expose", "OData"]
      },
      {
        title: "Metadata Extension",
        description: "Extensión de metadatos para UI en Fiori",
        code: `@Metadata.layer: #CUSTOMER
@UI: {
  headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title: {
      type: #STANDARD,
      value: 'TravelId'
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
  } ]

  @UI.lineItem: [ {
    position: 10,
    importance: #HIGH,
    label: 'Travel ID'
  } ]
  @UI.identification: [ {
    position: 10,
    label: 'Travel ID'
  } ]
  TravelId;

  @UI.lineItem: [ {
    position: 20,
    importance: #HIGH,
    label: 'Customer'
  } ]
  @UI.identification: [ {
    position: 20,
    label: 'Customer'
  } ]
  @Consumption.valueHelpDefinition: [{ entity: { name: 'Z_I_CUSTOMER', element: 'CustomerId' } }]
  CustomerId;
}`,
        tags: ["@Metadata", "@UI", "headerInfo", "facet", "lineItem"]
      }
    ]
  },
  "alv-reports": {
    title: "Reportes ALV",
    icon: "📋",
    prompts: [
      {
        title: "ALV Grid Display",
        description: "Mostrar datos en formato grid ALV",
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
        tags: ["ALV", "REUSE_ALV_GRID_DISPLAY", "fieldcat"]
      },
      {
        title: "ALV Object Model",
        description: "ALV usando modelo de objetos moderno",
        code: `DATA: lo_salv TYPE REF TO cl_salv_table.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table = lo_salv
  CHANGING
    t_table      = lt_data ).

" Configurar funciones
lo_salv->get_functions( )->set_all( ).

" Configurar columnas
DATA(lo_columns) = lo_salv->get_columns( ).
lo_columns->set_optimize( ).

" Mostrar
lo_salv->display( ).`,
        tags: ["cl_salv_table", "factory", "get_functions", "optimize"]
      }
    ]
  },
  "forms": {
    title: "Formularios",
    icon: "📄",
    prompts: [
      {
        title: "SmartForm - Definición",
        description: "Estructura básica de un SmartForm",
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
        tags: ["SmartForm", "SSF_FUNCTION_MODULE_NAME", "control_parameters"]
      }
    ]
  },
  "bapis": {
    title: "BAPIs y Function Modules",
    icon: "🔧",
    prompts: [
      {
        title: "BAPI Material - Crear",
        description: "Crear material usando BAPI",
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
        tags: ["BAPI", "BAPI_MATERIAL_SAVEDATA", "COMMIT", "ROLLBACK"]
      }
    ]
  },
  "modifications": {
    title: "Modificaciones",
    icon: "⚙️",
    prompts: [
      {
        title: "User-Exit Implementation",
        description: "Implementación básica de User-Exit",
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
        tags: ["User-Exit", "CASE", "MESSAGE", "MODIFY"]
      },
      {
        title: "BAdI Implementation",
        description: "Implementación de Business Add-In",
        code: `CLASS zcl_im_badi_example DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_badi_interface.

ENDCLASS.

CLASS zcl_im_badi_example IMPLEMENTATION.
  METHOD if_badi_interface~method_name.
    " Lógica de negocio personalizada
    IF input_parameter = 'CONDITION'.
      output_parameter = 'CUSTOM_RESULT'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`,
        tags: ["BAdI", "INTERFACES", "CLASS", "METHOD"]
      }
    ]
  },
  "performance": {
    title: "Performance y Optimización",
    icon: "⚡",
    prompts: [
      {
        title: "SELECT con FOR ALL ENTRIES",
        description: "Optimización usando FOR ALL ENTRIES",
        code: `" Verificar que la tabla interna no esté vacía
IF lt_vbeln IS NOT INITIAL.
  SELECT vbeln, posnr, matnr, kwmeng
    FROM vbap
    INTO TABLE @DATA(lt_items)
    FOR ALL ENTRIES IN @lt_vbeln
    WHERE vbeln = @lt_vbeln-vbeln.
ENDIF.

" Alternativa con FILTER
DATA(lt_filtered) = FILTER #( lt_items USING KEY vbeln
  WHERE vbeln = lv_sales_doc ).`,
        tags: ["FOR ALL ENTRIES", "FILTER", "Performance"]
      },
      {
        title: "Buffering de tablas",
        description: "Uso eficiente de buffer de tablas",
        code: `" Leer tabla bufferizada
SELECT SINGLE * FROM t001
  INTO @DATA(ls_company)
  WHERE bukrs = @lv_company_code.

" Usar READ TABLE para búsquedas múltiples
SORT lt_materials BY matnr.
READ TABLE lt_materials
  INTO DATA(ls_material)
  WITH KEY matnr = lv_material
  BINARY SEARCH.`,
        tags: ["Buffer", "READ TABLE", "BINARY SEARCH", "SORT"]
      }
    ]
  },
  "debugging": {
    title: "Debugging y Testing",
    icon: "🐛",
    prompts: [
      {
        title: "Breakpoints dinámicos",
        description: "Técnicas de debugging avanzado",
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
        tags: ["BREAK-POINT", "ASSERT", "MESSAGE", "Debugging"]
      },
      {
        title: "Unit Testing",
        description: "Estructura básica de unit test",
        code: `CLASS ltcl_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_material_processor.
    
    METHODS: setup,
             test_material_validation,
             test_price_calculation.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.
  
  METHOD test_material_validation.
    DATA(lv_result) = mo_cut->validate_material( 'TEST001' ).
    cl_abap_unit_assert=>assert_true(
      act = lv_result
      msg = 'Material validation failed' ).
  ENDMETHOD.
ENDCLASS.`,
        tags: ["Unit Test", "FOR TESTING", "cl_abap_unit_assert", "setup"]
      }
    ]
  }
};

function App() {
  const [selectedCategory, setSelectedCategory] = useState('seleccion-datos');
  const [searchTerm, setSearchTerm] = useState('');
  const [copiedCode, setCopiedCode] = useState('');

  const copyToClipboard = (code, title) => {
    navigator.clipboard.writeText(code);
    setCopiedCode(title);
    setTimeout(() => setCopiedCode(''), 2000);
  };

  const filteredPrompts = Object.entries(abapPrompts).reduce((acc, [key, category]) => {
    const filtered = category.prompts.filter(prompt => 
      prompt.title.toLowerCase().includes(searchTerm.toLowerCase()) ||
      prompt.description.toLowerCase().includes(searchTerm.toLowerCase()) ||
      prompt.tags.some(tag => tag.toLowerCase().includes(searchTerm.toLowerCase()))
    );
    if (filtered.length > 0) {
      acc[key] = { ...category, prompts: filtered };
    }
    return acc;
  }, {});

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
                <p className="text-gray-600">Los mejores prompts para desarrollo ABAP moderno</p>
              </div>
            </div>
          </div>
        </div>
      </header>

      <div className="container mx-auto px-6 py-8">
        <div className="flex flex-col lg:flex-row gap-8">
          {/* Sidebar */}
          <div className="lg:w-1/4">
            <div className="bg-white rounded-xl shadow-lg p-6 sticky top-8">
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

          {/* Main Content */}
          <div className="lg:w-3/4">
            {/* Search Bar */}
            <div className="bg-white rounded-xl shadow-lg p-6 mb-8">
              <div className="relative">
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
              {searchTerm && (
                <p className="mt-2 text-sm text-gray-600">
                  Mostrando resultados para: <span className="font-semibold">"{searchTerm}"</span>
                </p>
              )}
            </div>

            {/* Current Category Header */}
            {!searchTerm && (
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
              {(searchTerm ? Object.entries(filteredPrompts) : [[selectedCategory, abapPrompts[selectedCategory]]]).map(([categoryKey, category]) => (
                <div key={categoryKey}>
                  {searchTerm && (
                    <h3 className="text-xl font-bold text-gray-800 mb-4 flex items-center space-x-2">
                      <span>{category.icon}</span>
                      <span>{category.title}</span>
                    </h3>
                  )}
                  {category.prompts.map((prompt, index) => (
                    <div key={index} className="bg-white rounded-xl shadow-lg overflow-hidden hover:shadow-xl transition-shadow duration-300">
                      <div className="p-6">
                        <div className="flex justify-between items-start mb-4">
                          <div>
                            <h3 className="text-xl font-bold text-gray-800 mb-2">{prompt.title}</h3>
                            <p className="text-gray-600 mb-3">{prompt.description}</p>
                            <div className="flex flex-wrap gap-2">
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
                            className="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg transition-colors duration-200 flex items-center space-x-2"
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
            {searchTerm && Object.keys(filteredPrompts).length === 0 && (
              <div className="bg-white rounded-xl shadow-lg p-12 text-center">
                <div className="text-gray-400 mb-4">
                  <svg className="w-16 h-16 mx-auto" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9.172 16.172a4 4 0 015.656 0M9 12h6m-6-4h6m2 5.291A7.962 7.962 0 0112 15c-2.219 0-4.207.906-5.659 2.37M15 17h6l-3-3z" />
                  </svg>
                </div>
                <h3 className="text-xl font-bold text-gray-600 mb-2">No se encontraron resultados</h3>
                <p className="text-gray-500">Intenta con otros términos de búsqueda o explora las categorías disponibles.</p>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Footer */}
      <footer className="bg-gray-800 text-white py-8 mt-16">
        <div className="container mx-auto px-6 text-center">
          <p className="text-gray-400">© 2025 ABAP Prompts Wiki - Los mejores recursos para desarrollo ABAP moderno</p>
        </div>
      </footer>
    </div>
  );
}

export default App;