==========================================
Technical Manual of the ``Common`` Library
==========================================


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Author: Olivier Boudeville
:Contact: olivier.boudeville@esperide.com
:Creation Date: Wednesday, August 11, 2010
:Status: Work in progress
:Version: 0.1
:Abstract:

	The role of the ``Common`` library is to gather all Erlang general-purpose constructs that we found useful for Erlang developments, including Ceylan-based ones.


.. meta::
   :keywords: Common, generic, general-purpose, helper code



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 2

.. section-numbering::





:raw-latex:`\pagebreak`


------------------
Overview & Context
------------------

When using any programing language, there are always recurring patterns that prove useful. Instead of writing them again and again, we prefer to gather them all in a kind of lower-level library, in their most convenient, reliable, efficient version. This library provides its (generally simple) services just on top of the language.

As a consequence, for the Ceylan project, the first level of the software stack we use relies on this ``Common`` library, also known as the ``Ceylan Erlang library``.

The code involved remains very lightweight, i.e. simple and small (less than 10k lines).

.. comment Line count computed with: wc -l `find . -name '*.?rl'`



----------------
Usage Guidelines
----------------

License
=======

The ``Common`` library is licensed by its author (Olivier Boudeville) under a disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:

	- `Mozilla Public License <http://www.mozilla.org/MPL/MPL-1.1.html>`_ (MPL), version 1.1 or later (very close to the `Erlang Public License <http://www.erlang.org/EPLICENSE>`_, except aspects regarding Ericsson and/or the Swedish law)

	- `GNU General Public License <http://www.gnu.org/licenses/gpl-3.0.html>`_ (GPL), version 3.0 or later

	- `GNU Lesser General Public License <http://www.gnu.org/licenses/lgpl.html>`_ (LGPL), version 3.0 or later


This allows the use of the ``Common`` code in as wide a variety of software projects as possible, while still maintaining copyleft on this code.

Being triple-licensed means that someone (the licensee) who modifies and/or distributes it can choose which of the available sets of licence terms he is operating under.

Enhancements are expected to be back-contributed, so that everyone can benefit from them.



Recommended Usage & Contribution
================================

When developing Ceylan-based code, if needing a service already provided by this ``Common`` library, it is strongly advised to use that service and, possibly, expand or enrich it, with backward compatibility in mind.

If such a service is not provided by the current version of the library, yet being deemed generic enough, then it should preferably be directly added to the relevant part of the library and then called from the code that was needing it.

Of course, contributions of all sorts are welcome.

We do our best to test at least lightly each element provided. All services offered by ``foo.erl`` are thus expected to be tested in the companion ``foo_test.erl`` file.

Note that however we have not reached the discipline level of an exhaustive ``eunit`` test suite for each service.




:raw-latex:`\pagebreak`


-------------------------------
Services Offered By The Library
-------------------------------

The ``Common`` services are split into various themes:

 - general build structure
 - general settings
 - maths services
 - data-management services
 - helpers for graphical user interface (GUI) programming
 - all-purpose helper scripts
 - utility toolbox

Each of them will be detailed below, even if this is not an exhaustive walk-through.

A more detailed view of all the corresponding code is available in the generated `API documentation for Common <>`_.



:raw-latex:`\pagebreak`

General Build Structure
=======================

Various elements are defined at the ``Common`` level to set-up an appropriate build. They are to be used by this library, and used and enriched by all layers built on top of it.

This includes:

 - a set of pre-defined Make variables, describing various settings that will be reused by generic rules (ex: to compile modules with relevant flags, to create source archives, to install an application, to manage the various paths, etc.); these variables are defined in `common/src/GNUmakevars.inc <>`_

 - a set of generic rules, to compile and run various modules and tests, to generate various elements of documentation, etc.; these rules are defined in `common/src/GNUmakerules.inc <>`_

 - examples of minimal Make files, that mostly refer to the generic variables and rules; see ``common/src/GNUmakefile <>`_ as an example

These build facilities are designed to be enriched in turn by all layers above, which may add or override variables and rules.

An example of that is the `WOOPER <>`_ layer, directly built on top of ``Common``.


:raw-latex:`\pagebreak`

General Settings
================

These general-purpose settings deal with default CSS files, configuration files for various tools (ex: for ``Nedit``), etc.

They are gathered in the `common/conf <>`_ directory.



:raw-latex:`\pagebreak`

Maths Services
==============

Some maths-related operations are defined here:

 - the most basic services are centralised in `math_utils.erl <>`_:

   - general operations apparently lacking to Erlang (ex: ``floor/1``, ``ceiling/1``)or not implemented as we would have liked (ex: ``modulo/2``)

   - operations tailored to operate on floating-point values (ex: ``are_close/2``, ``is_null/1``)

   - operations on angles (ex: ``radian_to_degree/1``, ``canonify/1``)

 - linear-related operations are defined; for example the 2D operations are defined in `linear_2D.erl <>`_ and include:

   - operations on points: ``are_close/2``, ``is_within/3``, ``square_distance/2``, ``distance/2``, ``cross_product/2``, ``roundify/1``, ``get_integer_center/2``, ``get_center/2``, ``translate/2``, etc.

   - operations on vectors: ``vectorize/2``, ``square_magnitude/1``, ``magnitude/1``, ``scale/2``, ``make_unit/1``, ``normal_left/1``, ``normal_right/1``, ``dot_product/2``, etc.

   - operations on lines: ``get_line/2``, ``intersect/2``, ``get_abscissa_for_ordinate/2``, etc.

   - operations related to angles: ``is_strictly_on_the_right/3``, ``is_obtuse/1``, ``abs_angle_rad/3``, ``angle_rad/3``, ``abs_angle_deg/3``, ``angle_deg/3``, etc.

   - operations on sets of points: ``compute_smallest_enclosing_rectangle/1``, ``compute_max_overall_distance/1``, ``compute_convex_hull/1``, etc.

 - polygon-related operations are available in `polygon.erl <>`_:

   - generation of polygons: ``get_triangle/3``, ``get_upright_square/2``, ``get_polygon/1``, etc.

   - operations on them: ``get_diameter/1``, ``get_smallest_enclosing_rectangle/1``, ``get_area/1``, ``is_in_clockwise_order/1``, ``is_convex/1``, ``to_string/1``, etc.

   - rendering them: ``render/2``, ``set_edge_color/2``, ``get_edge_color/1``, ``set_fill_color/2``, ``get_fill_color/1``, etc.

   - managing their bounding boxes: ``update_bounding_box/2``, etc.

 - bounding-boxes in general are supported in `bounding_box.erl <>`_, including ``get_lazy_circle_box/1``, ``get_minimal_enclosing_circle_box/1``, etc.


All these services are gathered in the `common/src/maths <>`_ directory.



:raw-latex:`\pagebreak`

Data-Management Services
========================

Some generic data-structures, in addition to the ones provided built-in with Erlang, are defined here.

:raw-latex:`\pagebreak`

Helpers For Graphical User Interface Programing
===============================================


:raw-latex:`\pagebreak`

All-Purpose Helper Scripts
==========================


:raw-latex:`\pagebreak`

Utility Toolbox
===============
