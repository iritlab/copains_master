/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chatbot;

/**
 *
 * @author jorge
 */
//public class Combinations2 {
    
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Combinations2 {

    public static void main(String[] args) {
        // TODO Auto-generated method stub
        final String[][] array = new String[][] {
              new String[] {"ass(env,land)","ass(env,water)"}
            , new String[] {"ass(loc,indoor)","ass(loc,outdoor)","ass(loc,mixed)"}
            , new String[] {"ass(soc,single)","ass(soc,team)","ass(soc,mixed)"}
            , new String[] {"ass(cost,low)","ass(cost,med)","ass(cost,high)"}
            , new String[] {"ass(danger,low)","ass(danger,med)","ass(danger,high)"}                
            , new String[] {"ass(intens,low)","ass(intens,med)","ass(intens,high)"}                                
        };

        final List<String> list = combination(Arrays.asList(array));

        list.stream()
        .forEach(
            System.out::println
        )
        ;
    }

    static List<String> combination(
            final List<String[]> array)
    {
        if(array == null || array.size() < 1) {
            return Collections.<String>emptyList();
        }

        if(array.size() == 1) {
            return Arrays.stream(array.get(0))
                    .collect(Collectors.toList())
                    ;
        }

        if(array.size() == 2) {
            List<String> list1 = Arrays.stream(array.get(0))
                                .collect(Collectors.toList())
                                ;

            List<String> list2 = Arrays.stream(array.get(1))
                                .collect(Collectors.toList())
                                ;

            final List<String> list3 = new ArrayList<>();
            list1.stream()
            .forEach(
                s1 -> {
                    list2.stream()
                    .forEach(
                        s2 -> {
                            list3.add(s1 + ", " + s2);
                        }
                    );
                }
            )
            ;

            final List<String> list = new ArrayList<>();
            list.addAll(list1);
            list.addAll(list2);
            list.addAll(list3);

            return list;
        }

        //length > 2
        List<String> list = combination(array.subList(0, 2));

        for(int i=2; i<array.size(); i++) {
            List<String[]> newArrayList = new ArrayList<>();
            newArrayList.add(list.toArray(new String[list.size()]));
            newArrayList.add(array.get(i));

            list = combination(newArrayList);
        }

        return list;

    }
}
