(function(){

    var benchmarkRunList = undefined;
    var benchmarkList = undefined;
    var selectedRun = undefined;
    var timeResults = undefined;
    var speedupResults = undefined;

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
    
    function runListElementSelected(selectedRun){
        updateRunList(benchmarkRunList, selectedRun);
    }

    function benchmarkListElementSelected(selectedBenchmark){
        updatebenchmarkList(benchmarkList, selectedBenchmark);
    }

    function createNormalRunListElement(name){
        var $listElement = $('<li/>');
        
        $listElement.click( 
            function(){
                runListElementSelected($listElement.text());
            }
        );

        $listElement.text(name);

        return $listElement;
    }

    function createSelectedRunListElement(name){
        var $listElement = $('<li/>');
        $listElement.append($('<span/>').text(name).attr('style', 'text-decoration:overline;'));
        
        $listElement.append($('<br/>'));

        $.get(
            '/results/benchmarks_for_run',
            {run: name},
            function(benchmarkListParam){
                benchmarkList = benchmarkListParam;
                $('#benchmarkListDiv').replaceWith(createBenchmarkList(benchmarkList, benchmarkList[0]));
                $('#benchmarkListHeader').html("BENCHMARKS FOR RUN:<br/><i>" + name+"</i>");
            },
            'json');

        return $listElement;
    }


    function createNormalBenchmarkListElement(name){
        var $listElement = $('<li/>');
        
        $listElement.click( 
            function(){
                benchmarkListElementSelected(name);
            }
        );

        $listElement.text(name);

        return $listElement;
    }

    function createSelectedBenchmarkListElement(name){
        var $listElement = $('<li/>');
        $listElement.append($('<span/>').text(name).attr('style', 'text-decoration:overline;'));
        
        

        $.get(
            '/results/benchmark_results',
            {run: selectedRun,
             benchmark: name},
            function(benchmarkData){
                plotGraphs(benchmarkData);
            },
            'json');

        return $listElement;
    }


    function createBenchmarkList(benchmarkList, selectedBenchmark){
        var $benchmarkList = $('<ul/>').attr('id', 'benchmarkListDiv');
        $.each(
            benchmarkList,
            function(index, name){

                $listElement = undefined;
                
                if(name===selectedBenchmark){
                    $listElement = createSelectedBenchmarkListElement(name);
                } else {
                    $listElement = createNormalBenchmarkListElement(name);
                }

                $benchmarkList.append($listElement);

            });
        return $benchmarkList;
    }

    function createRunList(benchmarkRunList, selectedRun){
        var $benchmarkRunList = $('<ul/>').attr('id', 'benchmarkRunListDiv');
        $.each(
            benchmarkRunList,
            function(index, name){

                $listElement = undefined;
                
                if(name===selectedRun){
                    $listElement = createSelectedRunListElement(name);
                } else {
                    $listElement = createNormalRunListElement(name);
                }

                $benchmarkRunList.append($listElement);

            });
        return $benchmarkRunList;
    }
    
    function updateRunList(benchmarkRunListParam, selectedRunParam){
        if(benchmarkRunListParam === undefined){
            $.get(
                '/results/benchmark_runs_json',
                function(data){
                    updateRunList(data);
                }
                , 'json');
            return;
        }else{
            benchmarkRunList = benchmarkRunListParam;
        }
        if(selectedRunParam === undefined 
           && benchmarkRunListParam.length > 0){
            selectedRun = benchmarkRunList[0];
        }else{
            selectedRun = selectedRunParam;
        }
        $('#benchmarkRunListDiv').replaceWith(createRunList(benchmarkRunList, selectedRun));
    }


    function updatebenchmarkList(benchmarkList, selectedBenchmark){
        $('#benchmarkListDiv').replaceWith(createBenchmarkList(benchmarkList, selectedBenchmark));
    }


    $(document).ready(function(){
        
        updateRunList($.parseJSON($('#benchmarkRunListDiv').text()));
        
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

