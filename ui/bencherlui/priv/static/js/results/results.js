(function(){

    var selectedRun = undefined;
    var selectedBenchmark = undefined;
    var timeResults = undefined;
    var speedupResults = undefined;

    function plotGraphs(resultsParam){
        timeResults  = resultsParam;
        speedupResults = undefined;
        var i = 0;
        var choiceContainer = $("#overviewLegend");
        choiceContainer.empty();
        
        
        $.each(timeResults, function(key, val) {
            val.color = i;
            ++i;
            l = val.label;
            var li = $('<li />').appendTo(choiceContainer);

            $('<input name="' + l + '" id="' + l + '" type="checkbox" checked="checked" />').appendTo(li);
            $('<label>', {
                text: l,
                'for': l
            }).appendTo(li);
        });

        var previousPoint = null;

        $("#placeholder").bind("plothover", function(event, pos, item) {
            $("#x").text(pos.x.toFixed(2));
            $("#y").text(pos.y.toFixed(2));

            if (item) {
                if (previousPoint != item.datapoint) {
                    previousPoint = item.datapoint;

                    $("#tooltip").remove();
                    var x = item.datapoint[0].toFixed(2),
                    y = item.datapoint[1].toFixed(2);

                    showTooltip(item.pageX, item.pageY, item.series.label + " Value: " + y);
                }
            } else {
                $("#tooltip").remove();
                previousPoint = null;
            }
        });

        function showTooltip(x, y, contents) {
            $('<div id="tooltip">' + contents + '</div>').css({
                position: 'absolute',
                display: 'none',
                top: y + 5,
                left: x + 15,
                border: '1px solid #fdd',
                padding: '2px',
                backgroundColor: '#fee',
                opacity: 0.80
            }).appendTo("body").fadeIn(200);
        }

        plotAccordingToChoices();
        choiceContainer.find("input").change(plotAccordingToChoices);

        $('.legendColorBox > div').each(function(i) {
            $(this).clone().prependTo(choiceContainer.find("li").eq(i));
        });

    }
    
    function plotAccordingToChoices() {
        
        var graphType = $('input[name=graphTypeRadio]:checked').val();
        var results = undefined;
        if(graphType === "time"){
            results = timeResults;
        } else if(speedupResults !== undefined){
            results = speedupResults;
        } else {
            function convertTimeDataToSpeedupData(data){
                var timeOneSched = (jQuery.grep(data, function (a) { return a[0] === 1; }))[0][1];
                var newData = $.map( 
                    data, 
                    function(e){
                        e[1] = timeOneSched / e[1];
                        return e;
                    });
                return newData;
            }
            speedupResults = jQuery.extend(true, [], timeResults);
            $.each( speedupResults, function(index, graphObject){
                graphObject = convertTimeDataToSpeedupData(graphObject.data);
            } );
            results = speedupResults;
        }
        
        var options = {
            legend: {
                show: true
            },
            series: {
                points: {
                    show: true
                },
                lines: {
                    show: true
                }
            },
            grid: {
                hoverable: true
            }
        };
        
        var data = [];
        
        $("#overviewLegend").find("input:checked").each(function() {
            var key = this.name;

            for (var i = 0; i < results.length; i++) {
                if (results[i].label === key) {
                    data.push(results[i]);
                    return true;
                }
            }
        });

        $.plot($("#placeholder"), data, options);
    }

    function makeSelectable(item, action){
        item.selectable({
            selected: function(event, ui){            
                action($(ui.selected).text());
            }
        });
        var elements = item.find('li');
        if(elements.length > 0){
            var element = elements.first();
            element.addClass('ui-selected');
            action(element.text());
        }
    }


    function createSelectableList(nameList, selectEvent){
        var $list = $('<ul/>');
        $.each(
            nameList,
            function(index, name){
                $list.append($('<li/>').text(name));
            });
        makeSelectable($list, selectEvent);
        return $list;
    }

    function createRunList(benchmarkRunList){

        function elementSelected(name){

            selectedRun = name;

            $.get(
                '/results/benchmarks_for_run',
                {run: name},
                function(benchmarkList){
                    $('#benchmarkListDiv').replaceWith(
                        createBenchmarkList(benchmarkList).attr("id","benchmarkListDiv"));
                    $('#benchmarkListHeader').html("BENCHMARKS FOR:<br/><i>" + name+"</i>");
                },
                'json');

        }        
        
        return createSelectableList(benchmarkRunList, elementSelected);
    }
    

    function createBenchmarkList(benchmarkList){

        function elementSelected(name){

            selectedBenchmark = name;
 
            $.get(
                '/results/measurement_files',
                {run: selectedRun,
                 benchmark: name},
                function(measurementFileList){
                    $('#measurementFileListDiv').replaceWith(
                        createMeasurementFileList(measurementFileList).attr("id","measurementFileListDiv"));
                    $('#measurementFileListHeader').html("MEASUREMENTS FOR:<br/><i>" + name+"</i>");
                },
                'json');

        }        
        
        return createSelectableList(benchmarkList, elementSelected);
    }


    function createMeasurementFileList(measurementFileList){

        function elementSelected(name){

            $.get(
                '/results/benchmark_results',
                {run: selectedRun,
                 benchmark: selectedBenchmark,
                 measurementFile: name},
                function(benchmarkData){
                    plotGraphs(benchmarkData);
                },
                'json');

        }        
        
        return createSelectableList(measurementFileList, elementSelected);
    }

    $(document).ready(function(){
        
        var runList = $.parseJSON($('#benchmarkRunListDiv').text());

        $('#benchmarkRunListDiv').replaceWith(createRunList(runList));
        
        $("#graph").resizable();

        $("#graph").resize(function () {
            $('#placeholder').width($(this).width());
            $('#placeholder').height($(this).height());
            plotAccordingToChoices()
        });

        $( "#graphTypeRadio" ).buttonset();
        $( "#graphTypeRadio" ).click(plotAccordingToChoices);
        
    });    

})();

