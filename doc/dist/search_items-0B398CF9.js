searchNodes=[{"doc":"Module to handle nested maps.","ref":"maps_in.html","title":"maps_in","type":"module"},{"doc":"get/2.","ref":"maps_in.html#get/2","title":"maps_in.get/2","type":"function"},{"doc":"get/3.","ref":"maps_in.html#get/3","title":"maps_in.get/3","type":"function"},{"doc":"get_and_update/3.","ref":"maps_in.html#get_and_update/3","title":"maps_in.get_and_update/3","type":"function"},{"doc":"put/3.","ref":"maps_in.html#put/3","title":"maps_in.put/3","type":"function"},{"doc":"update/3.","ref":"maps_in.html#update/3","title":"maps_in.update/3","type":"function"},{"doc":"An Erlang library to handle nested maps.","ref":"readme.html","title":"maps_in","type":"extras"},{"doc":"General info Usage Build Test","ref":"readme.html#table-of-contents","title":"maps_in - Table of contents","type":"extras"},{"doc":"Erlang does not provide functions to handle nested maps, so this lib has this purpose and always uses a list of keys to manipulate maps.","ref":"readme.html#general-info","title":"maps_in - General info","type":"extras"},{"doc":"get/2 1 &gt; Map = \#{ my =&gt; \#{ nested =&gt; map } } . \#{ my =&gt; \#{ nested =&gt; map } } 2 &gt; maps_in : get ( [ my , nested ] , Map ) . map get/3 1 &gt; Map = \#{ my =&gt; \#{ nested =&gt; map } } . \#{ my =&gt; \#{ nested =&gt; map } } 2 &gt; maps_in : get ( [ my , unknown_key ] , Map , default ) . default put/3 1 &gt; Map = \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ } } } } . \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ } } } } 2 &gt; maps_in : put ( [ my , more , deep ] , \#{ nested =&gt; map } , Map ) . \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ nested =&gt; map } } } } update/3 1 &gt; Map = \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ } } } } . \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ } } } } 2 &gt; maps_in : update ( [ my , unknown_key ] , error , Map ) . * * exception error : bad key : unknown_key 3 &gt; maps_in : update ( [ my , more , deep ] , \#{ nested =&gt; map } , Map ) . \#{ my =&gt; \#{ more =&gt; \#{ deep =&gt; \#{ nested =&gt; map } } } } get_and_update/3 1 &gt; Map = \#{ someone =&gt; \#{ age =&gt; 17 } } . \#{ someone =&gt; \#{ age =&gt; 17 } } 2 &gt; maps_in : get_and_update ( [ someone , age ] , fun ( Age ) -&gt; Age + 1 end , Map ) . \#{ someone =&gt; \#{ age =&gt; 18 } }","ref":"readme.html#usage","title":"maps_in - Usage","type":"extras"},{"doc":"$ rebar3 compile","ref":"readme.html#build","title":"maps_in - Build","type":"extras"},{"doc":"$ rebar3 eunit","ref":"readme.html#test","title":"maps_in - Test","type":"extras"},{"doc":"Apache License Version 2.0, January 2004 http://www.apache.org/licenses/ TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION 1. Definitions. &quot;License&quot; shall mean the terms and conditions for use, reproduction, and distribution as defined by Sections 1 through 9 of this document. &quot;Licensor&quot; shall mean the copyright owner or entity authorized by the copyright owner that is granting the License. &quot;Legal Entity&quot; shall mean the union of the acting entity and all other entities that control, are controlled by, or are under common control with that entity. For the purposes of this definition, &quot;control&quot; means (i) the power, direct or indirect, to cause the direction or management of such entity, whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ownership of such entity. &quot;You&quot; (or &quot;Your&quot;) shall mean an individual or Legal Entity exercising permissions granted by this License. &quot;Source&quot; form shall mean the preferred form for making modifications, including but not limited to software source code, documentation source, and configuration files. &quot;Object&quot; form shall mean any form resulting from mechanical transformation or translation of a Source form, including but not limited to compiled object code, generated documentation, and conversions to other media types. &quot;Work&quot; shall mean the work of authorship, whether in Source or Object form, made available under the License, as indicated by a copyright notice that is included in or attached to the work (an example is provided in the Appendix below). &quot;Derivative Works&quot; shall mean any work, whether in Source or Object form, that is based on (or derived from) the Work and for which the editorial revisions, annotations, elaborations, or other modifications represent, as a whole, an original work of authorship. For the purposes of this License, Derivative Works shall not include works that remain separable from, or merely link (or bind by name) to the interfaces of, the Work and Derivative Works thereof. &quot;Contribution&quot; shall mean any work of authorship, including the original version of the Work and any modifications or additions to that Work or Derivative Works thereof, that is intentionally submitted to Licensor for inclusion in the Work by the copyright owner or by an individual or Legal Entity authorized to submit on behalf of the copyright owner. For the purposes of this definition, &quot;submitted&quot; means any form of electronic, verbal, or written communication sent to the Licensor or its representatives, including but not limited to communication on electronic mailing lists, source code control systems, and issue tracking systems that are managed by, or on behalf of, the Licensor for the purpose of discussing and improving the Work, but excluding communication that is conspicuously marked or otherwise designated in writing by the copyright owner as &quot;Not a Contribution.&quot; &quot;Contributor&quot; shall mean Licensor and any individual or Legal Entity on behalf of whom a Contribution has been received by Licensor and subsequently incorporated within the Work. 2. Grant of Copyright License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to reproduce, prepare Derivative Works of, publicly display, publicly perform, sublicense, and distribute the Work and such Derivative Works in Source or Object form. 3. Grant of Patent License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable (except as stated in this section) patent license to make, have made, use, offer to sell, sell, import, and otherwise transfer the Work, where such license applies only to those patent claims licensable by such Contributor that are necessarily infringed by their Contribution(s) alone or by combination of their Contribution(s) with the Work to which such Contribution(s) was submitted. If You institute patent litigation against any entity (including a cross-claim or counterclaim in a lawsuit) alleging that the Work or a Contribution incorporated within the Work constitutes direct or contributory patent infringement, then any patent licenses granted to You under this License for that Work shall terminate as of the date such litigation is filed. 4. Redistribution. You may reproduce and distribute copies of the Work or Derivative Works thereof in any medium, with or without modifications, and in Source or Object form, provided that You meet the following conditions: (a) You must give any other recipients of the Work or Derivative Works a copy of this License; and (b) You must cause any modified files to carry prominent notices stating that You changed the files; and (c) You must retain, in the Source form of any Derivative Works that You distribute, all copyright, patent, trademark, and attribution notices from the Source form of the Work, excluding those notices that do not pertain to any part of the Derivative Works; and (d) If the Work includes a &quot;NOTICE&quot; text file as part of its distribution, then any Derivative Works that You distribute must include a readable copy of the attribution notices contained within such NOTICE file, excluding those notices that do not pertain to any part of the Derivative Works, in at least one of the following places: within a NOTICE text file distributed as part of the Derivative Works; within the Source form or documentation, if provided along with the Derivative Works; or, within a display generated by the Derivative Works, if and wherever such third-party notices normally appear. The contents of the NOTICE file are for informational purposes only and do not modify the License. You may add Your own attribution notices within Derivative Works that You distribute, alongside or as an addendum to the NOTICE text from the Work, provided that such additional attribution notices cannot be construed as modifying the License. You may add Your own copyright statement to Your modifications and may provide additional or different license terms and conditions for use, reproduction, or distribution of Your modifications, or for any such Derivative Works as a whole, provided Your use, reproduction, and distribution of the Work otherwise complies with the conditions stated in this License. 5. Submission of Contributions. Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in the Work by You to the Licensor shall be under the terms and conditions of this License, without any additional terms or conditions. Notwithstanding the above, nothing herein shall supersede or modify the terms of any separate license agreement you may have executed with Licensor regarding such Contributions. 6. Trademarks. This License does not grant permission to use the trade names, trademarks, service marks, or product names of the Licensor, except as required for reasonable and customary use in describing the origin of the Work and reproducing the content of the NOTICE file. 7. Disclaimer of Warranty. Unless required by applicable law or agreed to in writing, Licensor provides the Work (and each Contributor provides its Contributions) on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You are solely responsible for determining the appropriateness of using or redistributing the Work and assume any risks associated with Your exercise of permissions under this License. 8. Limitation of Liability. In no event and under no legal theory, whether in tort (including negligence), contract, or otherwise, unless required by applicable law (such as deliberate and grossly negligent acts) or agreed to in writing, shall any Contributor be liable to You for damages, including any direct, indirect, special, incidental, or consequential damages of any character arising as a result of this License or out of the use or inability to use the Work (including but not limited to damages for loss of goodwill, work stoppage, computer failure or malfunction, or any and all other commercial damages or losses), even if such Contributor has been advised of the possibility of such damages. 9. Accepting Warranty or Additional Liability. While redistributing the Work or Derivative Works thereof, You may choose to offer, and charge a fee for, acceptance of support, warranty, indemnity, or other liability obligations and/or rights consistent with this License. However, in accepting such obligations, You may act only on Your own behalf and on Your sole responsibility, not on behalf of any other Contributor, and only if You agree to indemnify, defend, and hold each Contributor harmless for any liability incurred by, or claims asserted against, such Contributor by reason of your accepting any such warranty or additional liability. END OF TERMS AND CONDITIONS Copyright 2022, williamthome &lt;willegp88@gmail.com&gt;. Licensed under the Apache License, Version 2.0 (the &quot;License&quot;); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.","ref":"license.html","title":"LICENSE","type":"extras"}]