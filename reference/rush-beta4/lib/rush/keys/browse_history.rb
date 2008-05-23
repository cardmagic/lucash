module Rush
class HistoryBrowser < Key

  respond_to [96] # 96 = `

    def execute(ch,promptString)

      boe = -1  # gawd damn weird bug
      list = History.get.map do |m| 
        boe += 1
        Item.new(m[0..$bw.width-1],boe)
      end
      b = ComboBox.new($bw,$bw.cury,$bw.curx,10,list,"")
      b.run
      if b.accepted
        todelete = $bw.lines[-1].length-promptString.length
        0.upto(todelete){|i| $bw.backspace}
        $bw.type_string(History[b.items[b.item].tag])
      end
      b.destroy
      b = nil
      $bw.show_bottom
      $bw.clear
      $bw.display

    end

end
end # module
